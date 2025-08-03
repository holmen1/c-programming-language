#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/socket.h>
//#include "../include/http.h" nvim lsp
#include "http.h"

http_parse_e parse_http_headers(const char *raw_request, http_request *request) {
    const char *line_start = strstr(raw_request, "\r\n");
    if (!line_start) return HTTP_PARSE_INVALID;

    line_start += 2;
    while (line_start && *line_start && *line_start != '\r' && *line_start != '\n') {
        const char *line_end = strstr(line_start, "\r\n");
        if (!line_end) return HTTP_PARSE_INVALID;

        size_t len = line_end - line_start;
        char line[1024] = {0};
        strncpy(line, line_start, len);
        line[len] = '\0';

        char *colon = strchr(line, ':');
        if (colon) {
            *colon = '\0';
            const char *key = line;
            const char *value = colon + 1;
            while (*value == ' ') value++; // Skip leading spaces
            
            request->headers = (http_header_t *)realloc(request->headers, (request->header_count + 1) * sizeof(http_header_t));
            if (!request->headers) {
                perror("Failed to allocate memory for headers");
                exit(EXIT_FAILURE);
            }
            
            strncpy(request->headers[request->header_count].key, key, sizeof(request->headers[request->header_count].key)-1);
            strncpy(request->headers[request->header_count].value, value, sizeof(request->headers[request->header_count].value)-1);
            request->header_count++;
        }
        line_start = line_end + 2;
    }
    return HTTP_PARSE_OK;
}

http_parse_e read_http_request(int socket_fd, http_request *request) {
    ssize_t bytes_read = read(socket_fd, request->buffer, sizeof(request->buffer) - 1);

    if (bytes_read <= 0) {
        return HTTP_PARSE_INVALID; // Reading failed or connection closed
    }

    request->buffer[bytes_read] = '\0';

    // Create copy because strtok() modifies strings by inserting nulls; original needed for header parsing
    char buffer_copy[bytes_read + 1];
    strncpy(buffer_copy, request->buffer, bytes_read + 1);

    // Parse the request line
    // Example request line: "GET /index.html HTTP/1.1\r\n"
    char *method = strtok(buffer_copy, " ");
    char *path = strtok(NULL, " ");
    char *protocol = strtok(NULL, "\r\n");

    if (!method || !path || !protocol) {
        return HTTP_PARSE_INVALID; // Failed to parse the request line
    }

    strncpy(request->method, method, HTTP_METHOD_MAX_LEN);
    strncpy(request->path, path, HTTP_PATH_MAX_LEN);
    strncpy(request->protocol, protocol, HTTP_PROTOCOL_MAX_LEN);

    /* Alternative parsing using sscanf
    if (sscanf(buffer, "%7s %2047s %15s", request->method, request->path, request->protocol) != 3) {
        return -1; // Failed to parse the request line
    }
    */
    
    return HTTP_PARSE_OK;
}

void init_http_response(http_response *response) {
    response->status_code = 200; // Default to OK
    strncpy(response->reason_phrase, "OK", sizeof(response->reason_phrase) - 1);
    response->headers = NULL;
    response->header_count = 0;
    response->body = NULL;
    response->body_length = 0;
}

void add_http_header(http_response *response, const char *key, const char *value) {
    response->headers = realloc(response->headers, sizeof(http_header_t) * (response->header_count + 1));
    if (!response->headers) {
        perror("Failed to allocate memory for headers");
        exit(EXIT_FAILURE);
    }
    strncpy(response->headers[response->header_count].key, key, sizeof(response->headers[response->header_count].key) - 1);
    response->headers[response->header_count].key[sizeof(response->headers[response->header_count].key) - 1] = '\0';
    strncpy(response->headers[response->header_count].value, value, sizeof(response->headers[response->header_count].value) - 1);
    response->headers[response->header_count].value[sizeof(response->headers[response->header_count].value) - 1] = '\0';
    response->header_count++;
}

void set_http_body(http_response *response, const char *body) {
    if (!body) {
        return;
    }
    
    size_t body_len = strlen(body);
    
    response->body = malloc(body_len);
    if (!response->body) {
        perror("Failed to allocate memory for response body");
        exit(EXIT_FAILURE);
    }
    
    memcpy(response->body, body, body_len);
    response->body_length = body_len;

    char length_str[32];
    snprintf(length_str, sizeof(length_str), "%zu", body_len);
    add_http_header(response, "Content-Length", length_str);
}

char *construct_http_response(const http_response *response, size_t *response_length) {
    size_t buffer_size = 1024;
    char *buffer = malloc(buffer_size);
    if (!buffer) {
        perror("Failed to allocate memory for response buffer");
        exit(EXIT_FAILURE);
    }

    size_t offset = snprintf(buffer, buffer_size, "HTTP/1.1 %d %s\r\n", response->status_code, response->reason_phrase);

    for (size_t i = 0; i < response->header_count; i++) {
        size_t header_length = snprintf(NULL, 0, "%s: %s\r\n", response->headers[i].key, response->headers[i].value);
        while (offset + header_length + 1 > buffer_size) {
            buffer_size *= 2;
            buffer = realloc(buffer, buffer_size);
            if (!buffer) {
                perror("Failed to reallocate memory for response buffer");
                exit(EXIT_FAILURE);
            }
        }
        offset += snprintf(buffer + offset, buffer_size - offset, "%s: %s\r\n", response->headers[i].key, response->headers[i].value);
    }

    offset += snprintf(buffer + offset, buffer_size - offset, "\r\n");

    if (response->body) {
        while (offset + response->body_length + 1 > buffer_size) {
            buffer_size *= 2;
            buffer = realloc(buffer, buffer_size);
            if (!buffer) {
                perror("Failed to reallocate memory for response buffer");
                exit(EXIT_FAILURE);
            }
        }
        memcpy(buffer + offset, response->body, response->body_length);
        offset += response->body_length;
    }

    *response_length = offset;
    return buffer;
}

void serve_file(const char *path, http_response *response) {
    FILE *file = fopen(path, "rb");
    if (!file) {
        response->status_code = 404;
        strncpy(response->reason_phrase, "Not Found", sizeof(response->reason_phrase) - 1);
        file = fopen("./www/error.html", "rb");
    }

    // Determine the file size
    fseek(file, 0, SEEK_END);
    size_t file_size = ftell(file);
    fseek(file, 0, SEEK_SET);

    // Allocate memory for the file content
    char *file_content = malloc(file_size + 1);
    if (!file_content) {
        perror("Failed to allocate memory for file content");
        fclose(file);
        exit(EXIT_FAILURE);
    }

    fread(file_content, 1, file_size, file);
    fclose(file);
    file_content[file_size] = '\0';

    // Set the response body
    response->body = file_content;
    response->body_length = file_size;

    // Determine content type (basic implementation)
    if (strstr(path, ".html")) {
        add_http_header(response, "Content-Type", "text/html");
    } else if (strstr(path, ".css")) {
        add_http_header(response, "Content-Type", "text/css");
    } else if (strstr(path, ".js")) {
        add_http_header(response, "Content-Type", "application/javascript");
    } else if (strstr(path, ".png")) {
        add_http_header(response, "Content-Type", "image/png");
    } else {
        add_http_header(response, "Content-Type", "application/octet-stream");
    }

    // Add content length header
    char content_length[32];
    snprintf(content_length, sizeof(content_length), "%zu", file_size);
    add_http_header(response, "Content-Length", content_length);
}

void send_http_response(int client_fd, const http_response *response) {
    size_t response_length = 0;
    char *response_data = construct_http_response(response, &response_length);

    size_t total_sent = 0;
    while (total_sent < response_length) {
        ssize_t bytes_sent = send(client_fd, response_data + total_sent, response_length - total_sent, 0);
        if (bytes_sent <= 0) {
            perror("Failed to send response");
            break;
        }
        total_sent += bytes_sent;
    }

    free(response_data);
}

void sanitize_path(const char *requested_path, char *sanitized_path, size_t buffer_size) {
    const char *web_root = "./www";
    if (strcmp(requested_path, "/") == 0)
        snprintf(sanitized_path, buffer_size, "%s/%s", web_root, "index.html");
    else
        snprintf(sanitized_path, buffer_size, "%s%s", web_root, requested_path);

    // Prevent directory traversal by normalizing the path
    if (strstr(sanitized_path, "..")) {
        strncpy(sanitized_path, "./www/error.html", buffer_size - 1); // Serve a 404 page
    }
}

void free_http_headers(http_request *request) {
    free(request->headers);
    request->headers = NULL;
    request->header_count = 0;
}

void free_http_response(http_response *response) {
    free(response->headers);
    response->headers = NULL;
    response->header_count = 0;

    free(response->body);
    response->body = NULL;
    response->body_length = 0;
}


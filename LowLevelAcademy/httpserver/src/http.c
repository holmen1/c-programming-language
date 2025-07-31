#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include "../include/http.h"

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

void free_http_headers(http_request *request) {
    free(request->headers);
    request->headers = NULL;
    request->header_count = 0;
}


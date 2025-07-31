#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include "../include/http.h"

void parse_http_headers(const char *raw_request, http_request *request) {
    const char *p = strstr(raw_request, "\r\n");
    if (!p) return;
    p += 2;
    int count = 0;
    const char *q = p;
    while (q && strncmp(q, "\r\n", 2) != 0 && *q) {
        count++;
        q = strstr(q, "\r\n");
        if (q) q += 2;
    }
    if (count == 0) return;
    request->headers = calloc(count, sizeof(http_header_t));
    request->header_count = count;
    for (int i = 0; i < count; i++) {
        const char *next = strstr(p, "\r\n");
        size_t len = next ? (size_t)(next - p) : strlen(p);
        char line[768] = {0};
        strncpy(line, p, len);
        line[len] = '\0';
        char *colon = strchr(line, ':');
        if (colon) {
            *colon = '\0';
            strncpy(request->headers[i].key, line, sizeof(request->headers[i].key)-1);
            strncpy(request->headers[i].value, colon+1, sizeof(request->headers[i].value)-1);
        }
        p = next ? next + 2 : NULL;
    }
}

int read_http_request(int socket_fd, http_request *request) {
    char buffer[HTTP_METHOD_MAX_LEN + HTTP_PATH_MAX_LEN + HTTP_PROTOCOL_MAX_LEN + 3] = {0};
    ssize_t bytes_read = read(socket_fd, buffer, sizeof(buffer) - 1);

    if (bytes_read <= 0) {
        return -1; // Reading failed or connection closed
    }

    buffer[bytes_read] = '\0';

    // Ensure the buffer is null-terminated, save for http headers parsing
    char buffer_copy[sizeof(buffer)];
    strncpy(buffer_copy, buffer, sizeof(buffer_copy));
    buffer_copy[sizeof(buffer_copy) - 1] = '\0';

    // Parse the request line
    // Example request line: "GET /index.html HTTP/1.1\r\n"
    char *method = strtok(buffer, " ");
    char *path = strtok(NULL, " ");
    char *protocol = strtok(NULL, "\r\n");

    if (!method || !path || !protocol) {
        return -1; // Failed to parse the request line
    }

    strncpy(request->method, method, HTTP_METHOD_MAX_LEN);
    strncpy(request->path, path, HTTP_PATH_MAX_LEN);
    strncpy(request->protocol, protocol, HTTP_PROTOCOL_MAX_LEN);

    /* Alternative parsing using sscanf
    if (sscanf(buffer, "%7s %2047s %15s", request->method, request->path, request->protocol) != 3) {
        return -1; // Failed to parse the request line
    }
    */
    parse_http_headers(buffer_copy, request);
    return 0;
}


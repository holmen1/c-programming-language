#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include "http.h"

int read_http_request(int socket_fd, http_request *request) {
    char buffer[HTTP_METHOD_MAX_LEN + HTTP_PATH_MAX_LEN + HTTP_PROTOCOL_MAX_LEN + 3] = {0};
    ssize_t bytes_read = read(socket_fd, buffer, sizeof(buffer) - 1);

    if (bytes_read <= 0) {
        return -1; // Reading failed or connection closed
    }

    buffer[bytes_read] = '\0';

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

    return 0;
}
#include <stdlib.h>
#include <stdio.h>
#include "../include/common.h"
#include "../include/tcp.h"
#include "../include/http.h"


void handle_client(int client_fd) {
    http_request req = {0};

    if (read_http_request(client_fd, &req) == HTTP_PARSE_INVALID) {
        debug_log("Failed to read or parse HTTP request");
        close(client_fd);
        return;
    }

    if (parse_http_headers(req.buffer, &req) == HTTP_PARSE_INVALID) {
        debug_log("Failed to read or parse HTTP request");
        close(client_fd);
        return;
    }

    debug_log("HTTP request parsed successfully");
    debug_log(req.method);
    debug_log(req.path);
    debug_log(req.protocol);

    char header_buf[800];
    for (int i = 0; i < req.header_count; ++i) {
        snprintf(header_buf, sizeof(header_buf), "Key: %s\tValue: %s", req.headers[i].key, req.headers[i].value);
        debug_log(header_buf);
    }

    free_http_headers(&req);
    close(client_fd);
}


int main() {
    tcp_server server = {0};
    server_status_e status = bind_tcp_port(&server, 8080);
    if (status != SERVER_OK) {
        debug_log("Server initialization failed");
        exit(EXIT_FAILURE);
    }

    int client_fd = accept_client(server.socket_fd);
    if (client_fd == -1) {
        debug_log("Failed to accept client connection");
        close(server.socket_fd);
        exit(EXIT_FAILURE);
    }

    debug_log("Client connected");

    handle_client(client_fd);

    close(server.socket_fd);
    return 0;
}

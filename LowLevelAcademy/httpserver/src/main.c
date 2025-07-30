#include <stdlib.h>
#include "../include/common.h"
#include "../include/tcp.h"
#include "../include/http.h"


void handle_client(int client_fd) {
    http_request req = {0};

    if (read_http_request(client_fd, &req) == -1) {
        debug_log("Failed to read or parse HTTP request");
        close(client_fd);
        return;
    }

    debug_log("HTTP request parsed successfully");
    debug_log(req.method);
    debug_log(req.path);
    debug_log(req.protocol);

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

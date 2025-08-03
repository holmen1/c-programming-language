#include <stdlib.h>
#include <stdio.h>
#include "../include/common.h"
#include "../include/tcp.h"
#include "../include/http.h"


void handle_client(int client_fd) {
    size_t buffer_size = 1024;
    http_request req = {0};
    http_response res = {0};

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

    char sanitized_path[buffer_size];
    sanitize_path(req.path, sanitized_path, buffer_size);
    

    
    init_http_response(&res);
    serve_file(sanitized_path, &res);
    send_http_response(client_fd, &res);

    
    debug_log("Response sent and client connection closed");
    free_http_headers(&req);
    free_http_response(&res);
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

/*
$ ./bin/httpserver 
Server bound and listening on port 8080
DEBUG: Client connected
DEBUG: HTTP request parsed successfully
DEBUG: GET
DEBUG: /
DEBUG: HTTP/1.1
DEBUG: Key: Host        Value: localhost:8080
DEBUG: Key: User-Agent  Value: curl/8.14.1
DEBUG: Key: Accept      Value: /
DEBUG: Response sent and client connection closed

$ curl localhost:8080
<html><body><h1>Hello, world!</h1></body></html>
*/

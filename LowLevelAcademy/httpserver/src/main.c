#include "common.h"
#include "http.h"
#include "route.h"
#include "tcp.h"
#include <stdio.h>
#include <stdlib.h>

void handle_client(int client_fd) {
  size_t buffer_size = 1024;
  http_request_raw req_raw = {0};
  http_request req = {0};
  http_response res = {0};

  if (read_http_request(client_fd, &req_raw) == HTTP_PARSE_INVALID) {
    debug_log("Failed to read HTTP request");
    close(client_fd);
    return;
  }

  if (parse_http_request(&req_raw, &req) == HTTP_PARSE_INVALID) {
    debug_log("Failed to parse HTTP request");
    close(client_fd);
    return;
  }

  char sanitized_path[buffer_size];
  sanitize_path(req.path, sanitized_path, buffer_size);

  init_http_response(&res);

  if (!handle_request(&req, &res))
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

  size_t no_of_routes;
  no_of_routes = install_routes();
  no_of_routes > 0 ? debug_log("Routes installed")
                   : debug_log("No routes installed");

  for (;;) {
    int client_fd = accept_client(server.socket_fd);
    if (client_fd == -1) {
      debug_log("Failed to accept client connection");
      close(server.socket_fd);
      exit(EXIT_FAILURE);
    }

    debug_log("Client connected");

    handle_client(client_fd);
  }

  close(server.socket_fd);
  return 0;
}

/*
$ ./bin/httpserver
Server bound and listening on port 8080
DEBUG: Routes installed
DEBUG: Client connected
DEBUG: Response sent and client connection closed
DEBUG: Client connected
DEBUG: Response sent and client connection closed

$ curl localhost:8080/hello
Hello World!
$ curl localhost:8080/
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Welcome</title>
</head>
<body>
    <h1>Welcome to the Toy HTTP Server!</h1>
    <p>This is the index page.</p>
</body>
</html>
*/

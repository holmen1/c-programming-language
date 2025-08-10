#include "http.h"
#include "route.h"
#include <string.h>
#include <stdlib.h>

Route routes[MAX_ROUTES];
int route_count = 0;

void hello_handler(http_request *req, http_response *res);

static void install_route(http_method_e method, const char *path,
                     void (*handler)(http_request *req, http_response *res)) {
  if (route_count < MAX_ROUTES) {
    routes[route_count].method = method;
    strcpy(routes[route_count].path, path);
    routes[route_count].handler = handler;
    ++route_count;
  }
}

size_t install_routes() {
  install_route(GET, "/hello", hello_handler);   
  return route_count;
}

void hello_handler(http_request *req, http_response *res) {
  (void)req; // Unused in this handler
  res->status_code = 200;

  if (!res->body)
    res->body = malloc(64);

  strcpy(res->body, "Hello World!\n");
  res->body_length = 13;
}

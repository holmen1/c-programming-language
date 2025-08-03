#ifndef HTTP_H
#define HTTP_H

#define HTTP_REQUEST_MAX_LEN 8192*4
#define HTTP_METHOD_MAX_LEN 8
#define HTTP_PATH_MAX_LEN 2048
#define HTTP_PROTOCOL_MAX_LEN 16

#define HTTP_MAX_HEADER_KEY_LEN 256
#define HTTP_MAX_HEADER_VALUE_LEN 256

typedef enum {
    HTTP_PARSE_OK,
    HTTP_PARSE_INVALID,
} http_parse_e;

typedef struct {
    char key[HTTP_MAX_HEADER_KEY_LEN];
    char value[HTTP_MAX_HEADER_VALUE_LEN];
} http_header_t;

typedef struct {
    char method[HTTP_METHOD_MAX_LEN];
    char path[HTTP_PATH_MAX_LEN];
    char protocol[HTTP_PROTOCOL_MAX_LEN];
    http_header_t *headers; // heap-allocated array
    int header_count;
    char buffer[HTTP_REQUEST_MAX_LEN];
} http_request;

http_parse_e read_http_request(int socket_fd, http_request *request);
http_parse_e parse_http_headers(const char *raw_request, http_request *request);
void free_http_headers(http_request *request);

#endif

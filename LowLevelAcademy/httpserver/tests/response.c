#include "http.h"
#include <stdio.h>
#include <string.h>

int main() {
    http_response response;
    init_http_response(&response);

    printf("Initialized HTTP Response:\n");
    printf("Status Code: %d\n", response.status_code);
    printf("Reason Phrase: %s\n", response.reason_phrase);

    return 0;
}

// $ gcc -I../include -o test_response response.c ../obj/http.o
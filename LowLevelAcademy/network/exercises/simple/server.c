#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define PORT 5555
#define BACKLOG 5

typedef enum {
	PROTO_HELLO,
} proto_type_e;

typedef struct {
	proto_type_e type;
	unsigned int len;
} proto_hdr_t;

void handle_client(int fd) {
	char buf[4096] = {0};
	proto_hdr_t *hdr = (proto_hdr_t*)buf;

	hdr->type = htonl(PROTO_HELLO); /* pack the type */
	hdr->len = sizeof(int);
	int reallen = hdr->len;
	hdr->len = htons(hdr->len); /* pack the len */

	int *data = (int*)&hdr[1];
	*data = htonl(1); /* protocol version one, packed */
	write(fd, hdr, sizeof(proto_hdr_t) + reallen);
}

int main(void) {
    struct sockaddr_in serverInfo = {0};
    struct sockaddr_in clientInfo = {0};
    socklen_t clientInfoLen = sizeof(clientInfo);

    serverInfo.sin_family = AF_INET;
    serverInfo.sin_addr.s_addr = INADDR_ANY;
    serverInfo.sin_port = htons(PORT);

    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd == -1) {
        perror("socket");
        return 0;
    }

    /* Bind */
    if (bind(fd, (struct sockaddr *)&serverInfo, sizeof(serverInfo)) == -1) {
        perror("bind");
        close(fd);
        return 0;
    }

    /* Listen */
    if (listen(fd, BACKLOG) == -1) {
        perror("listen");
        close(fd);
        return 0;
    }

    while (1) {
      int cfd = accept(fd, (struct sockaddr*)&clientInfo, &clientInfoLen);
      if (cfd == -1) {
        perror("accept");
        close(fd);
        return 0;
      }
      
      handle_client(cfd);

      close(cfd);
    }
}

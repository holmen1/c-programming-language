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

void handle_server(int fd) {
	char buf[4096] = {0};
	proto_hdr_t *hdr = (proto_hdr_t*)buf;
	read(fd, hdr, sizeof(proto_hdr_t) + sizeof(int));
	hdr->type = ntohl(hdr->type); /* unpack the type */
	hdr->len = ntohs(hdr->len);

	int *data = (int*)&hdr[1];
	*data = ntohl(*data); /* protocol version one, packed */

	if (*data != 1) {
		printf("Protocol mismatch!\n");
		return;
	}

	printf("Successfully connected to the server, protocol v1\n");
	return;
}

int main(int argc, char *argv[]) {
    if (argc != 2) {
        printf("Usage: %s <ip of host>\n", argv[0]);
        return 0;
    }

    struct sockaddr_in serverInfo = {0};

    serverInfo.sin_family = AF_INET;
    serverInfo.sin_addr.s_addr = inet_addr(argv[1]);
    serverInfo.sin_port = htons(PORT);

    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd == -1) {
        perror("socket");
        return 0;
    }

    if (connect(fd, (struct sockaddr*)&serverInfo, sizeof(serverInfo)) == -1) {
        perror("connect");
        close(fd);
        return 0;
    }

    handle_server(fd);

    close(fd);
    return 0;
}

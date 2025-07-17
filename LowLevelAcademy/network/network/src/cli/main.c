#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <stdio.h>
#include <unistd.h>
#include <getopt.h>

#include "common.h"

int send_hello(int fd) {
    char buf[4096] = {0};

	dbproto_hdr_t *hdr = (dbproto_hdr_t *)buf;
	hdr->type = MSG_HELLO_REQ;
	hdr->len = 1;

	dbproto_hello_req *hello_req = (dbproto_hello_req *)&hdr[1];
	hello_req->proto = PROTO_VER;

	hdr->type = htonl(hdr->type);
	hdr->len = htons(hdr->len);
	hello_req->proto = htons(hello_req->proto);

	write(fd, buf, sizeof(dbproto_hdr_t) + sizeof(dbproto_hello_req));
	printf("Sent hello request to server. Protocol v1\n");
	read(fd, buf, sizeof(buf));

	hdr->type = ntohl(hdr->type);
	hdr->len = ntohs(hdr->len);

	if (hdr->type != MSG_HELLO_RESP) {
		fprintf(stderr, "Unexpected response type: %d\n", hdr->type);
		return STATUS_ERROR;
	}
	printf("Received hello response from server. Protocol v%d\n", ntohs(hello_req->proto));
	return STATUS_SUCCESS;
}

int main(int argc, char *argv[]) {
	char *portarg = NULL;
	char *hostarg = NULL;
	char *addarg = NULL; 
	unsigned short port = 0;
	
	int c;
	while ((c = getopt(argc, argv, "h:p:a:")) != -1) {
		switch (c) {
			case 'h':
				hostarg = optarg;
				break;
			case 'p':
				portarg = optarg;
				port = atoi(portarg);
				break;
			case 'a':
				addarg = optarg;
				break;
			case '?':
				fprintf(stderr, "Unknown option -%c\n", c);
				break;
			default:
				return -1;

		}
	}

	if (hostarg == NULL) {
		fprintf(stderr, "Host is a required argument\n");
		fprintf(stderr, "Usage: %s -p <port> -h <host> [-a <addarg>]\n", argv[0]);
		return -1;
	}

	if (port == 0) {
		fprintf(stderr, "Bad port: %s\n", portarg);
		fprintf(stderr, "Usage: %s -p <port> -h <host> [-a <addarg>]\n", argv[0]);
		return -1;
	}

    struct sockaddr_in serverInfo = {0};

    serverInfo.sin_family = AF_INET;
    serverInfo.sin_addr.s_addr = inet_addr(hostarg);
    serverInfo.sin_port = htons(port);

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

    if (send_hello(fd) != STATUS_SUCCESS) {
		close(fd);
		return -1;
	}

    close(fd);
    return 0;
}
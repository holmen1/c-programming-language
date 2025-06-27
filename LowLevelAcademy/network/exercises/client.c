#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <netinet/in.h>

#define PORT 5555
#define BACKLOG 5

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

    close(fd);
    return 0;
}

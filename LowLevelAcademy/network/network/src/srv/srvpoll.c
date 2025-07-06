#include <arpa/inet.h>
#include <poll.h>
#include <stdio.h>
#include <sys/time.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "srvpoll.h"

void init_clients(clientstate_t *states) {
    int i = 0;
    for (; i < MAX_CLIENTS; i++) {
        states[i].fd = -1; /* -1 indicating free slot */
        states[i].state = STATE_NEW;
        memset(states[i].buffer, '\0', BUFF_SIZE); /* Clear the buffer */
    }
}

int find_free_slot(clientstate_t *states) {
    int i = 0;
    for (; i < MAX_CLIENTS; i++) {
        if (states[i].fd == -1) {
            return i; /* Return the index of the first free slot */
        }
    }
    return -1; /* No free slot found */
}

int find_slot_by_fd(clientstate_t *states, int fd) {
    int i = 0;
    for (; i < MAX_CLIENTS; i++) {
        if (states[i].fd == fd) {
            return i;
        }
    }
    return -1; /* No slot found for the given file descriptor */
}

int setup_server_socket(unsigned short port) {
    int listen_fd;
    struct sockaddr_in server_addr;
    int opt = 1;

    /* Create server socket */
    if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        perror("socket");
        exit(EXIT_FAILURE);
    }

    if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) < 0) {
        perror("setsockopt");
        close(listen_fd);
        exit(EXIT_FAILURE);
    }

    /* Prepare server address structure */
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(port);

    /* Bind socket to port */
    if (bind(listen_fd, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0) {
        perror("bind");
        close(listen_fd);
        exit(EXIT_FAILURE);
    }

    /* Listen for incoming connections */
    if (listen(listen_fd, BACKLOG) < 0) {
        perror("listen");
        close(listen_fd);
        exit(EXIT_FAILURE);
    }
    
    return listen_fd;
}
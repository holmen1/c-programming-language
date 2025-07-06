#include <arpa/inet.h>
#include <poll.h>
#include <stdio.h>
#include <sys/time.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "srvpoll.h"
#include "parse.h"

clientstate_t clientStates[MAX_CLIENTS];
static volatile int keep_running = 1;

static void init_clients(clientstate_t *states) {
    int i = 0;
    for (; i < MAX_CLIENTS; i++) {
        states[i].fd = -1; /* -1 indicating free slot */
        states[i].state = STATE_NEW;
        memset(states[i].buffer, '\0', BUFF_SIZE); /* Clear the buffer */
    }
}

static int find_free_slot(clientstate_t *states) {
    int i = 0;
    for (; i < MAX_CLIENTS; i++) {
        if (states[i].fd == -1) {
            return i; /* Return the index of the first free slot */
        }
    }
    return -1; /* No free slot found */
}

static int find_slot_by_fd(clientstate_t *states, int fd) {
    int i = 0;
    for (; i < MAX_CLIENTS; i++) {
        if (states[i].fd == fd) {
            return i;
        }
    }
    return -1; /* No slot found for the given file descriptor */
}

/**
 * Sets up the server socket to listen for incoming connections.
 * 
 * @param port The port number to bind the server socket to.
 * @return The file descriptor of the listening socket.
 */
static int setup_server_socket(unsigned short port) {
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

/**
 * Prepares the poll file descriptors for the server and active clients.
 * 
 * @param fds Array of pollfd structures to be filled.
 * @param listen_fd The listening socket file descriptor.
 * @param nfds Pointer to an integer that will hold the number of file descriptors.
 */
static void prepare_poll_fds(struct pollfd *fds, int listen_fd, int *nfds) {
    int i, poll_index = 1;
    
    /* Clear the pollfd array */
    memset(fds, 0, sizeof(struct pollfd) * (MAX_CLIENTS + 1));
    
    /* Set up server socket for polling */
    fds[0].fd = listen_fd;
    fds[0].events = POLLIN;
    
    /* Add active clients to the poll array */
    for (i = 0; i < MAX_CLIENTS; i++) {
        if (clientStates[i].fd != -1) {
            fds[poll_index].fd = clientStates[i].fd;
            fds[poll_index].events = POLLIN;
            poll_index++;
        }
    }
    
    *nfds = poll_index;  /* Update nfds */
}

/**
 * Handles a new incoming connection.
 * 
 * @param listen_fd The listening socket file descriptor.
 */
static void handle_new_connection(int listen_fd) {
    int conn_fd, freeSlot;
    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);
    
    if ((conn_fd = accept(listen_fd, (struct sockaddr *)&client_addr, &client_len)) == -1) {
        perror("accept");
        return;
    }

    printf("New connection from %s:%d\n", 
           inet_ntoa(client_addr.sin_addr), ntohs(client_addr.sin_port));

    freeSlot = find_free_slot(clientStates);
    if (freeSlot == -1) {
        printf("Server full: closing new connection\n");
        close(conn_fd);
    } else {
        clientStates[freeSlot].fd = conn_fd;
        clientStates[freeSlot].state = STATE_CONNECTED;
        printf("Client connected in slot %d with fd %d\n", freeSlot, conn_fd);
    }
}

/**
 * Handles incoming data from a client.
 * 
 * @param fd The file descriptor of the client socket.
 */
static void handle_client_data(int fd) {
    int slot = find_slot_by_fd(clientStates, fd);
    if (slot == -1) {
        close(fd);  /* Unknown client - just close it */
        return;
    }
    
    ssize_t bytes_read = read(fd, clientStates[slot].buffer, 
                             sizeof(clientStates[slot].buffer) - 1);
                             
    if (bytes_read > 0) {
        clientStates[slot].buffer[bytes_read] = '\0';  /* Null-terminate */
        printf("Received from client %d: %s\n", slot, clientStates[slot].buffer);
        /* TODO: Process commands here */
    } else {
        /* Connection closed or error */
        close(fd);
        clientStates[slot].fd = -1;
        clientStates[slot].state = STATE_DISCONNECTED;
        printf("Client in slot %d disconnected\n", slot);
    }
}

static void handle_signal(int sig) {
    printf("Received signal %d, shutting down...\n", sig);
    keep_running = 0;
}

void poll_loop(unsigned short port, struct dbheader_t *dbhdr, struct employee_t *employees) {
    signal(SIGINT, handle_signal);
    signal(SIGTERM, handle_signal);
    int i, listen_fd;
    int n_events;
    int nfds;
    struct pollfd fds[MAX_CLIENTS + 1];
    
    /* Initialize client states */
    init_clients(clientStates);
    
    /* Set up server socket */
    listen_fd = setup_server_socket(port);
    printf("Server started on port %d. Connect with: nc localhost %d\n", port, port);
    
    while (keep_running) {
        /* Prepare poll fds for this iteration */
        prepare_poll_fds(fds, listen_fd, &nfds);
        
        /* Wait for events */
        n_events = poll(fds, nfds, 30000);
        
        if (n_events < 0) {
            perror("poll");
            break;  /* Exit the loop on error */
        }
        
        if (n_events == 0) {
            printf("Poll timeout - no activity\n");
            continue;
        }
        
        /* Handle new connections */
        if (fds[0].revents & POLLIN) {
            handle_new_connection(listen_fd);
            n_events--;
        }
        
        /* Handle client activity */
        for (i = 1; i < nfds && n_events > 0; i++) {
            if (fds[i].revents & POLLIN) {
                handle_client_data(fds[i].fd);
                n_events--;
            }
        }
    }
    
    /* Clean up */
    close(listen_fd);
}
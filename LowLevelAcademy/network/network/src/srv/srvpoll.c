#include <arpa/inet.h>
#include <poll.h>
#include <stdio.h>
#include <sys/time.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "common.h"
#include "srvpoll.h"
#include "parse.h"

clientstate_t clientStates[MAX_CLIENTS];
static volatile int keep_running = 1;

static void init_clients(clientstate_t *states);
static int setup_server_socket(unsigned short port);
static void prepare_poll_fds(struct pollfd *fds, int listen_fd, int *nfds);
static int find_free_slot(clientstate_t *states);
static int find_slot_by_fd(clientstate_t *states, int fd);
static void handle_signal(int sig);
static void handle_client_fsm(struct dbheader_t *dbhdr, struct employee_t *employees, clientstate_t *client);
static void fsm_reply_hello_err(clientstate_t *client, dbproto_hdr_t *hdr);
static void fsm_reply_hello(clientstate_t *client, dbproto_hdr_t *hdr);

void poll_loop(unsigned short port, struct dbheader_t *dbhdr, struct employee_t *employees) {
    int conn_fd, freeSlot;
    struct sockaddr_in client_addr;
    socklen_t client_len = sizeof(client_addr);

    int i, listen_fd;
    int n_events;
    int nfds;
    struct pollfd fds[MAX_CLIENTS + 1];
    signal(SIGINT, handle_signal);
    signal(SIGTERM, handle_signal);
    
    init_clients(clientStates);
    
    listen_fd = setup_server_socket(port);
    printf("Server started on port %d. Connect with: nc localhost %d\n", port, port);
    
    while (keep_running) {
        /*
        Set up the array of file descriptors to be monitored by the poll() system call
        clears the entire array, then adds the server's listening socket at index 0,
        followed by all active client connections */
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

        /* Handle new connection */
        if (fds[0].revents & POLLIN) {
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
                clientStates[freeSlot].state = STATE_HELLO;
                printf("Client connected in slot %d with fd %d\n", freeSlot, conn_fd);
                nfds++;
            }
            n_events--;
        }
        
        /* Handle client activity */
        for (i = 1; i <= nfds && n_events > 0; i++) { /* Start from 1 to skip the listen_fd */
            if (fds[i].revents & POLLIN) {
                n_events--;

                int fd = fds[i].fd;
                int slot = find_slot_by_fd(clientStates, fd);
                ssize_t bytes_read = read(fd, &clientStates[slot].buffer, sizeof(clientStates[slot].buffer));
                if (bytes_read <= 0) {
                    /* Connection closed or error */
                    close(fd);

                    if (slot != -1) {
                        clientStates[slot].fd = -1; /* Free up the slot */
                        clientStates[slot].state = STATE_DISCONNECTED;
                        printf("Client disconnected\n");
                        nfds--;
                    }
                } else {
                   handle_client_fsm(dbhdr, employees, &clientStates[slot]);
                }
            }
        }

        if (!keep_running) {
            printf("Shutting down server...\n");
            break;
        }
    }
    
    printf("Closing server socket...\n");
    close(listen_fd);
}



static void init_clients(clientstate_t *states) {
    int i = 0;
    for (; i < MAX_CLIENTS; i++) {
        states[i].fd = -1; /* -1 indicating free slot */
        states[i].state = STATE_NEW;
        memset(states[i].buffer, '\0', BUFF_SIZE); /* Clear the buffer */
    }
}

/* listen for incoming connections, return the file descriptor of the listening socket */
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

static void handle_client_fsm(struct dbheader_t *dbhdr, struct employee_t *employees, clientstate_t *client) {
    dbproto_hdr_t *hdr = (dbproto_hdr_t *)client->buffer;

    hdr->type = ntohl(hdr->type);
    hdr->len = ntohs(hdr->len);

    if (client->state == STATE_HELLO) {
        if (hdr->type != MSG_HELLO_REQ || hdr->len != 1) {
            printf("Client %d sent unexpected message type %d in HELLO state\n", client->fd, hdr->type);
            /* TODO: send error msg */
        }

        dbproto_hello_req *hello_req = (dbproto_hello_req *)&hdr[1];
        hello_req->proto = ntohs(hello_req->proto);
        if (hello_req->proto != PROTO_VER) {
            printf("Client %d sent unsupported protocol version %d\n", client->fd, hello_req->proto);
            fsm_reply_hello_err(client, hdr);
            return;
        }

        fsm_reply_hello(client, hdr);
        client->state = STATE_MSG;  /* Move to MSG state */
        printf("Client %d sent valid hello request, moving to MSG state\n", client->fd);
    }

    if (client->state == STATE_MSG) {
        if (hdr->type == MSG_EMPLOYEE_ADD_REQ) {

            dbproto_employee_add_req *employee = (dbproto_employee_add_req *)&hdr[1];
            printf("Adding employee: %s\n", employee->data);
            fsm_reply_add(client, hdr);
        }
    }
}

static void fsm_reply_hello_err(clientstate_t *client, dbproto_hdr_t *hdr) {
    hdr->type = htonl(MSG_ERROR);
    hdr->len = htons(0);

    write(client->fd, hdr, sizeof(dbproto_hdr_t));
}

static void fsm_reply_hello(clientstate_t *client, dbproto_hdr_t *hdr) {
    hdr->type = htonl(MSG_HELLO_RESP);
    hdr->len = htons(1);
    dbproto_hello_resp* hello = (dbproto_hello_req*)&hdr[1];
    hello->proto = htons(PROTO_VER);

    write(client->fd, hdr, sizeof(dbproto_hdr_t) + sizeof(dbproto_hello_resp));
}

void fsm_reply_add(clientstate_t *client, dbproto_hdr_t *hdr) {
    hdr->type = htonl(MSG_EMPLOYEE_ADD_RESP);
    hdr->len = htons(0);

    write(client->fd, hdr, sizeof(dbproto_hdr_t));
}

static void handle_signal(int sig) {
    printf("Received signal %d, shutting down...\n", sig);
    keep_running = 0;
}
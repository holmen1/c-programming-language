#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>

#include "common.h"
#include "file.h"
#include "parse.h"
#include "srvpoll.h"


#define MAX_CLIENTS 256
#define BACKLOG 10

clientstate_t clientStates[MAX_CLIENTS] = {0};

void print_usage(char *argv[]) {
    printf("Usage: %s -n -f <database_file>\n", argv[0]);
    printf("Options:\n");
    printf("\t -n          create new database file\n");
    printf("\t -f <file>   (required) database file path\n");
    printf("\t -p <port>   (required) port to listen on\n");
	printf("\t -a <string> Add a new employee with the given string format 'name,address,hours'\n");
	printf("\t -l          List all employees in the database\n");
	printf("\t -d <name>    Delete employee with the given name\n");
    printf("\t -e <string>  Edit employee with format 'name,new_address,new_hours'\n");
    exit(0);
}


void poll_loop(unsigned short port, struct dbheader_t *dbhdr, struct employee_t *employees) {
    int listen_fd, conn_fd, freeSlot;
    struct sockaddr_in server_addr, client_addr;
    socklen_t client_len = sizeof(client_addr);

    struct pollfd fds[MAX_CLIENTS + 1];  /* +1 for server socket */
    int nfds = 1;  /* Number of file descriptors to monitor (start with server) */
    int opt = 1;  /* Option for setsockopt */

    /* Initialize client states */
    init_clients(&client_states);

    /* Create server socket */
    if ((listen_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
        perror("socket");
        exit(EXIT_FAILURE);

    if (setsockopt(listen_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) < 0) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }

    /* Prepare server address structure */
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;  /* Accept connections from any IP */
    server_addr.sin_port = htons(port);  /* Convert port number to network byte order */

    /* Bind socket to port */
    if (bind(listen_fd, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0) {
        perror("bind");
        close(listen_fd);
        exit(EXIT_FAILURE);
    }

    /* Listen for incoming connections */
    if (listen(listen_fd, BACKLOG) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }
    
    printf("Server started on port %d. Connect with: nc localhost %d\n", PORT, PORT);

    /* Initialize the pollfd array with server socket */
    memset(fds, 0, sizeof(fds));  /* Clear the pollfd array */
    fds[0].fd = listen_fd;
    fds[0].events = POLLIN;  /* Interested in read events */
    nfds = 1;

    while (1) {

        int ii = 1;
        for (int i = 0; i < MAX_CLIENTS; i++) {
            if (clientStates[i].fd != -1) {
                fds[ii].fd = clientStates[i].fd; /* Offset by 1 for listen_fd */
                fds[ii].events = POLLIN;
                ii++;
            }   
        }

        // Wait for an event on one of the sockets
        int n_events = poll(fds, nfds, -1); // -1 means no timeout
        if (n_events == -1) {
            perror("poll");
            exit(EXIT_FAILURE);
        }

        // Check for new connections
        if (fds[0].revents & POLLIN) {
            if ((conn_fd = accept(listen_fd, (struct sockaddr *)&client_addr, &client_len)) == -1) {
                perror("accept");
                continue;
            }

            printf("New connection from %s:%d\n",
            	inet_ntoa(client_addr.sin_addr), ntohs(client_addr.sin_port));

            freeSlot = find_free_slot(&clientStates);
            if (freeSlot == -1) {
                printf("Server full: closing new connection\n");
                close(conn_fd);
            } else {
                clientStates[freeSlot].fd = conn_fd;
                clientStates[freeSlot].state = STATE_HELLO;
                nfds++;
                printf("Slot %d has fd %d\n", freeSlot, clientStates[freeSlot].fd);
            }

            n_events--;
        }

        // Check each client for read/write activity
        for (int i = 1; i <= nfds && n_events > 0; i++) { // Start from 1 to skip the listen_fd
            if (fds[i].revents & POLLIN) {
                n_events--;

                int fd = fds[i].fd;
                int slot = find_slot_by_fd(&clientStates, fd);
                ssize_t bytes_read = read(fd, &clientStates[slot].buffer, sizeof(clientStates[slot].buffer));
                if (bytes_read <= 0) {
                    // Connection closed or error
                    close(fd);

                    if (slot != -1) {
                        clientStates[slot].fd = -1; // Free up the slot
                        clientStates[slot].state = STATE_DISCONNECTED;
                        printf("Client disconnected\n");
                        nfds--;
                    }
                } else {
                    handle_client_fsm(dbhdr, employees, &clientStates[slot]);
                }
            }
        }
    }

}


int main(void) {
    int server_fd, client_fds[MAX_CLIENTS];
    struct pollfd fds[MAX_CLIENTS + 1];  /* +1 for server socket */
    int nfds = 1;  /* Number of file descriptors to monitor (start with server) */
    int activity, i, new_socket, bytes_read;
    char buffer[BUFFER_SIZE];
    struct sockaddr_in server_addr, client_addr;
    socklen_t addr_len = sizeof(client_addr);
    
    /* Initialize client array with -1 (no connection) */
    for (i = 0; i < MAX_CLIENTS; i++) {
        client_fds[i] = -1;
    }
    
    /* Create server socket */
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        perror("socket");
        exit(EXIT_FAILURE);
    }
    
    /* Set socket options to allow reuse of address/port
       Prevents "Address already in use" errors */
    int opt = 1;
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt)) < 0) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }
    
    /* Prepare server address structure */
    memset(&server_addr, 0, sizeof(server_addr));
    server_addr.sin_family = AF_INET;
    server_addr.sin_addr.s_addr = INADDR_ANY;
    server_addr.sin_port = htons(PORT);
    
    /* Bind socket to port */
    if (bind(server_fd, (struct sockaddr *)&server_addr, sizeof(server_addr)) < 0) {
        perror("bind");
        exit(EXIT_FAILURE);
    }
    
    /* Listen for incoming connections */
    if (listen(server_fd, BACKLOG) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }
    
    printf("Server started on port %d. Connect with: nc localhost %d\n", PORT, PORT);
    
    /* Initialize the pollfd array with server socket */
    fds[0].fd = server_fd;
    fds[0].events = POLLIN;  /* Interested in read events */

    /* Initialize remaining slots as unused */
    for (i = 1; i <= MAX_CLIENTS; i++) {
        fds[i].fd = -1;
    }
    
    while (1) {
        
        printf("Waiting for activity...\n");
        activity = poll(fds, nfds, -1);  /* -1 means wait indefinitely */
        
        if (activity < 0) {
            perror("select");
            break;
        }
        
        /* Check if there's activity on the server socket (new connection) */
        if (fds[0].revents & POLLIN) {
            if ((new_socket = accept(server_fd, (struct sockaddr *)&client_addr, &addr_len)) < 0) {
                perror("accept");
                continue;
            }
            
            printf("New connection from %s:%d on socket fd %d\n", 
                   inet_ntoa(client_addr.sin_addr), ntohs(client_addr.sin_port), new_socket);
            
            /* Add new client to an empty slot */
            int added = 0;
            for (i = 0; i < MAX_CLIENTS; i++) {
                if (client_fds[i] == -1) {
                    client_fds[i] = new_socket;
                    added = 1;
                    break;
                }
            }
            
            if (!added) {
                printf("Too many clients, connection rejected\n");
                close(new_socket);
            } else {
                /* Add to poll set */
                fds[i+1].fd = new_socket;
                fds[i+1].events = POLLIN;
                if (i+1 >= nfds) {
                    nfds = i+2;  /* Update the number of fds to monitor */
                }
                
                /* Welcome message */
                char welcome[] = "Welcome to the echo server! Type something and press enter.\n";
                send(new_socket, welcome, strlen(welcome), 0);
            }
        }
        
        /* Check for activity on client sockets */
        for (i = 1; i < nfds; i++) {
            if (fds[i].fd != -1 && (fds[i].revents & POLLIN)) {
                /* Data available from client */
                bytes_read = read(fds[i].fd, buffer, BUFFER_SIZE - 1);
                
                if (bytes_read <= 0) {
                    /* Connection closed or error */
                    if (bytes_read == 0) {
                        printf("Client on fd %d disconnected\n", fds[i].fd);
                    } else {
                        perror("read");
                    }
                    
                    /* Close socket and remove from poll set */
                    close(fds[i].fd);
                    
                    /* Find and clear the corresponding client_fds entry */
                    int j = 0;
                    for (; j < MAX_CLIENTS; j++) {
                        if (client_fds[j] == fds[i].fd) {
                            client_fds[j] = -1;
                            break;
                        }
                    }
                    
                    /* Mark the pollfd slot as unused */
                    fds[i].fd = -1;
                    
                    /* Compact the array if this was the last entry */
                    if (i == nfds - 1) {
                        nfds--;
                        /* Find new last valid entry */
                        while (nfds > 1 && fds[nfds-1].fd == -1) {
                            nfds--;
                        }
                    }
                } else {
                    /* Echo back the message */
                    buffer[bytes_read] = '\0';
                    printf("Received from fd %d: %s", fds[i].fd, buffer);
                    
                    /* Add a prefix to show it's coming back from the server */
                    char response[BUFFER_SIZE + 32];
                    sprintf(response, "Server echoes: %s", buffer);
                    send(fds[i].fd, response, strlen(response), 0);
                }
            }
        }
    }
    
    /* Clean up */
    for (i = 0; i < MAX_CLIENTS; i++) {
        if (client_fds[i] != -1) {
            close(client_fds[i]);
        }
    }
    close(server_fd);
    
    return 0;
}
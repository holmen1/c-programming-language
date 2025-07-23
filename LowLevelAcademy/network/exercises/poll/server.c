#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <poll.h>
#include <netinet/in.h>

#define PORT 5555
#define BUFFER_SIZE 1024
#define MAX_CLIENTS 5
#define BACKLOG 5

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
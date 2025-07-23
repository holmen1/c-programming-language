#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <netinet/in.h>

#define PORT 5555
#define BUFFER_SIZE 1024
#define MAX_CLIENTS 5
#define BACKLOG 5

int main(void) {
    int server_fd, client_fds[MAX_CLIENTS];
    fd_set read_fds, master_fds;
    int max_fd, activity, i, new_socket, bytes_read;
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
    
    /* Initialize the file descriptor sets */
    FD_ZERO(&master_fds);
    FD_SET(server_fd, &master_fds);
    max_fd = server_fd;
    
    while (1) {
        /* Copy the master set to the read set (select modifies the set) */
        read_fds = master_fds;
        
        printf("Waiting for activity...\n");
        activity = select(max_fd + 1, &read_fds, NULL, NULL, NULL);
        
        if (activity < 0) {
            perror("select");
            break;
        }
        
        /* Check if there's activity on the server socket (new connection) */
        if (FD_ISSET(server_fd, &read_fds)) {
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
                /* Add to master set */
                FD_SET(new_socket, &master_fds);
                if (new_socket > max_fd) {
                    max_fd = new_socket;
                }
                
                /* Welcome message */
                char welcome[] = "Welcome to the echo server! Type something and press enter.\n";
                send(new_socket, welcome, strlen(welcome), 0);
            }
        }
        
        /* Check for activity on client sockets */
        for (i = 0; i < MAX_CLIENTS; i++) {
            if (client_fds[i] != -1 && FD_ISSET(client_fds[i], &read_fds)) {
                /* Data available from client */
                bytes_read = read(client_fds[i], buffer, BUFFER_SIZE - 1);
                
                if (bytes_read <= 0) {
                    /* Connection closed or error */
                    if (bytes_read == 0) {
                        printf("Client on fd %d disconnected\n", client_fds[i]);
                    } else {
                        perror("read");
                    }
                    
                    /* Close socket and remove from set */
                    close(client_fds[i]);
                    FD_CLR(client_fds[i], &master_fds);
                    client_fds[i] = -1;
                } else {
                    /* Echo back the message */
                    buffer[bytes_read] = '\0';
                    printf("Received from fd %d: %s", client_fds[i], buffer);
                    
                    /* Add a prefix to show it's coming back from the server */
                    char response[BUFFER_SIZE + 32];
                    sprintf(response, "Server echoes: %s", buffer);
                    send(client_fds[i], response, strlen(response), 0);
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
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdbool.h>
#include <string.h>
#include <arpa/inet.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/in.h>
#include <getopt.h>

#include "common.h"
#include "file.h"
#include "parse.h"
#include "srvpoll.h"


#define MAX_CLIENTS 256

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
    int i, listen_fd, conn_fd, freeSlot;
    struct sockaddr_in server_addr, client_addr;
    socklen_t client_len = sizeof(client_addr);

    struct pollfd fds[MAX_CLIENTS + 1];  /* +1 for server socket */
    int nfds = 1;  /* Number of file descriptors to monitor (start with server) */
    int opt = 1;  /* Option for setsockopt */

    /* Initialize client states */
    init_clients(clientStates);

    listen_fd = setup_server_socket(port);
    printf("Server started on port %d. Connect with: nc localhost %d\n", port, port);

    /* Initialize the pollfd array with server socket */
    memset(fds, 0, sizeof(fds));  /* Clear the pollfd array */
    fds[0].fd = listen_fd;
    fds[0].events = POLLIN;  /* Interested in read events */

    while (1) {

        int poll_index = 1; /* Start at 1 for listen_fd */
        for (i = 0; i < MAX_CLIENTS; i++) {
            if (clientStates[i].fd != -1) {
                fds[poll_index].fd = clientStates[i].fd;
                fds[poll_index].events = POLLIN;
                poll_index++;
            }   
        }
        nfds = poll_index; /* Set nfds to exactly match the number of valid entries */

        /* Wait for an event on one of the sockets */
        int n_events = poll(fds, nfds, 30000); /* 30 s timeout */
        if (n_events < 0) {
            perror("poll");
            exit(EXIT_FAILURE);
        }
        if (n_events == 0) {
            /* Timeout occurred - no activity for 30 seconds */
            printf("Poll timeout - no activity\n");
            continue; /* Skip this iteration and go back to polling */
        }

        /* Check for new connections */
        if (fds[0].revents & POLLIN) {
            if ((conn_fd = accept(listen_fd, (struct sockaddr *)&client_addr, &client_len)) == -1) {
                perror("accept");
                continue;
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
                printf("Slot %d has fd %d\n", freeSlot, clientStates[freeSlot].fd);
            }

            n_events--;
        }

        /* Check each client for read/write activity */
        for (i = 1; i <= nfds && n_events > 0; i++) { /* Start from 1 to skip the listen_fd */
            if (fds[i].revents & POLLIN) {
                n_events--;

                int fd = fds[i].fd;
                int slot = find_slot_by_fd(clientStates, fd);
                ssize_t bytes_read = read(fd, clientStates[slot].buffer, sizeof(clientStates[slot].buffer) - 1);
                if (bytes_read > 0)
                    clientStates[slot].buffer[bytes_read] = '\0';  /* Null-terminate */
                if (bytes_read <= 0) {
                    /* Connection closed or error */
                    close(fd);

                    if (slot != -1) {
                        clientStates[slot].fd = -1; /* Free up the slot */
                        clientStates[slot].state = STATE_DISCONNECTED;
                        printf("Client disconnected\n");
                    }
                } else {
                    printf("TODO\n");
                }
            }
        }
    }
}

int main(int argc, char *argv[]) { 
	char *filepath = NULL;
	char *portarg = NULL;
	unsigned short port = 0;
	bool newfile = false;
	int c;
    /*
    // Uncomment this section if you want to use file descriptors directly
	int dbfd = -1;
    */
	struct dbheader_t *dbhdr = NULL;
	struct employee_t *employees = NULL;

    
	while ((c = getopt(argc, argv, "nf:p:")) != -1) {
		switch (c) {
			case 'n':
				newfile = true;
				break;
			case 'f':
				filepath = optarg;
				break;
			case 'p':
				portarg = optarg;
				port = atoi(portarg);
				if (port == 0) {
					printf("Bad port: %s\n", portarg);
				}
				break;
			case '?':
				printf("Unknown option -%c\n", c);
			default:
				return -1;

		}
	}

	if (filepath == NULL) {
		printf("Filepath is a required argument\n");
		print_usage(argv);

		return 0;
	}

	if (port == 0) {
		printf("Port not set\n");
		print_usage(argv);
		return 0;
	}
/*

	if (newfile) {
		dbfd = create_db_file(filepath);
		if (dbfd == STATUS_ERROR) {
			printf("Unable to create database file\n");
			return -1;
		}

		if (create_db_header(dbfd, &dbhdr) == STATUS_ERROR) {
			printf("Failed to create database header\n"); 
			return -1;
		}

	} else {
		dbfd = open_db_file(filepath);
		if (dbfd == STATUS_ERROR) {
			printf("Unable to open database file\n");
			return -1;
		}

		if (validate_db_header(dbfd, &dbhdr) == STATUS_ERROR) {
			printf("Failed to validate database header\n");
			return -1;
		}
	}

	if (read_employees(dbfd, dbhdr, &employees) != STATUS_SUCCESS) {
		printf("Failed to read employees");
		return 0;
	}
    */
	poll_loop(port, dbhdr, employees);

	return 0;
}
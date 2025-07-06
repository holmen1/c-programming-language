#ifndef SRVPOLL_H
#define SRVPOLL_H
#include <poll.h>


#define MAX_CLIENTS 256
#define BUFF_SIZE 4096
#define BACKLOG 10 /* Maximum number of pending connections in the queue */

typedef enum {
    STATE_NEW,
    STATE_CONNECTED,
    STATE_DISCONNECTED
} state_e;

/* Structure to hold client state */
typedef struct {
    int fd;                 /* File descriptor for the client socket */
    state_e state;          /* Current state of the client */
    char buffer[BUFF_SIZE]; /* Buffer for incoming data */
} clientstate_t;

void init_clients(clientstate_t *states);
int find_free_slot(clientstate_t *states);
int find_slot_by_fd(clientstate_t *states, int fd);
/*static int setup_server_socket(unsigned short port);*/
int setup_server_socket(unsigned short port);

#endif /* SRVPOLL_H */
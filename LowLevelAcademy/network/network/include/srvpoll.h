#ifndef SRVPOLL_H
#define SRVPOLL_H
#include <poll.h>


#define MAX_CLIENTS 256
#define PORT 8080
#define BUFF_SIZE 4096

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

#endif /* SRVPOLL_H */
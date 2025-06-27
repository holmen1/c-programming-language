#ifndef SRVPOLL_H
#define SRVPOLL_H

#include <poll.h>
#include "parse.h"


#define MAX_CLIENTS 256
#define BUFF_SIZE 4096
#define BACKLOG 10 /* Maximum number of pending connections in the queue */

typedef enum {
    STATE_NEW,
    STATE_DISCONNECTED,
    STATE_HELLO,
    STATE_MSG
} state_e;

/* Structure to hold client state */
typedef struct {
    int fd;                 /* File descriptor for the client socket */
    state_e state;          /* Current state of the client */
    char buffer[BUFF_SIZE]; /* Buffer for incoming data */
} clientstate_t;

void poll_loop(unsigned short port, struct dbheader_t *dbhdr, struct employee_t **employees, int dbfd);

#endif /* SRVPOLL_H */
#include <arpa/inet.h>
#include <poll.h>
#include <sys/time.h>

#include "srvpoll.h"

void init_clients(clientstate_t *states) {
    for (int i = 0; i < MAX_CLIENTS; i++) {
        states[i].fd = -1; /* -1 indicating free slot */
        states[i].state = STATE_NEW;
        memset(states[i].buffer, '\0', BUFF_SIZE); /* Clear the buffer */
    }
}

int find_free_slot(clientstate_t *states) {
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (states[i].fd == -1) {
            return i; /* Return the index of the first free slot */
        }
    }
    return -1; /* No free slot found */
}

int find_slot_by_fd(clientstate_t *states, int fd) {
    for (int i = 0; i < MAX_CLIENTS; i++) {
        if (states[i].fd == fd) {
            return i;
        }
    }
    return -1; /* No slot found for the given file descriptor */
}
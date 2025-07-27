#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#define NUM_PHILOSOPHERS 5

pthread_mutex_t forks[NUM_PHILOSOPHERS];

void* philosopher(void* num) {
    int id = *(int*)num;
    int left = id;
    int right = (id + 1) % NUM_PHILOSOPHERS;

    while (1) {
        printf("Philosopher %d is thinking.\n", id);
        sleep(1);

        printf("Philosopher %d is hungry.\n", id);

        /* Try to pick up the left fork */
        pthread_mutex_lock(&forks[left]);
        printf("Philosopher %d picked up left fork %d.\n", id, left);

        /* Try to pick up the right fork */
        pthread_mutex_lock(&forks[right]);
        printf("Philosopher %d picked up right fork %d.\n", id, right);

        /* Simulate eating */
        printf("Philosopher %d is eating.\n", id);
        sleep(1);

        /* Put down the right fork */
        pthread_mutex_unlock(&forks[right]);
        printf("Philosopher %d put down right fork %d.\n", id, right);

        /* Put down the left fork */
        pthread_mutex_unlock(&forks[left]);
        printf("Philosopher %d put down left fork %d.\n", id, left);
    }
}

int main() {
    pthread_t philosophers[NUM_PHILOSOPHERS];
    int ids[NUM_PHILOSOPHERS];

    /* Initialize the forks (mutexes) */
    int i = 0;
    for (; i < NUM_PHILOSOPHERS; i++) {
        pthread_mutex_init(&forks[i], NULL);
    }

    /* Create philosopher threads */
    for (i = 0; i < NUM_PHILOSOPHERS; i++) {
        ids[i] = i;
        pthread_create(&philosophers[i], NULL, philosopher, &ids[i]);
    }

    /* Wait for all philosophers to finish (in this case, they never will) */
    for (i = 0; i < NUM_PHILOSOPHERS; i++) {
        pthread_join(philosophers[i], NULL);
    }

    /* Destroy the forks (mutexes) */
    for (i = 0; i < NUM_PHILOSOPHERS; i++) {
        pthread_mutex_destroy(&forks[i]);
    }

    return 0;
}

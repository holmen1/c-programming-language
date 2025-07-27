#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>


pthread_mutex_t lock;

void startup_function() {
    pthread_mutex_init(&lock, NULL);
}

void *thread_function(void *arg) {
    pthread_mutex_lock(&lock);
    *(int *)arg += 1;
    pthread_mutex_unlock(&lock);

    return NULL;
}

int main() {
    pthread_t threads[10];
    int counter = 0;
    
    startup_function();
    
    int i = 0;
    for (; i < 10; i++) {
        if (pthread_create(&threads[i], NULL, thread_function, &counter) != 0) {
            fprintf(stderr, "Error creating thread %d\n", i);
            return 1;
        }
    }
    
    for (i = 0; i < 10; i++) {
        pthread_join(threads[i], NULL);
    }
    
    printf("%d\n", counter);
    
    pthread_mutex_destroy(&lock);
    
    return 0;
}
#define _GNU_SOURCE
#include <pthread.h>
#include <sched.h>
#include <stdio.h>

void *thread_function(void *arg) {
  pthread_t tid = pthread_self();
  cpu_set_t cpus;
  CPU_ZERO(&cpus);

  if (pthread_getaffinity_np(tid, sizeof(cpu_set_t), &cpus) == 0) {
    printf("Thread %lu is running on CPU ", tid);
    int i = 0;
    for (; i < CPU_SETSIZE; i++) {
      if (CPU_ISSET(i, &cpus))
        printf("%d ", i);
    }
    printf("\n");
  }

  return NULL;
}

int main() {
  pthread_t thread;
  pthread_attr_t attr;
  cpu_set_t cpus;
  pthread_attr_init(&attr);
  CPU_ZERO(&cpus);
  CPU_SET(0, &cpus); /* Set thread to run on CPU 0 */
  CPU_SET(2, &cpus); /* Set thread to run on CPU 2 */

  pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpus);
  pthread_create(&thread, &attr, thread_function, NULL);
  pthread_join(thread, NULL);
  pthread_attr_destroy(&attr);

  return 0;
}

/*
* $ gcc -std=c90 -Wall affinity.c -pthread
*/

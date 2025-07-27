#define _GNU_SOURCE
#include <pthread.h>
#include <sched.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

void pin_to_core(int core_id) {
  cpu_set_t cpus;
  pthread_t current_thread;
  
  current_thread = pthread_self();
  CPU_ZERO(&cpus);
  CPU_SET(core_id, &cpus);

  pthread_setaffinity_np(current_thread, sizeof(cpu_set_t), &cpus);
}
#include <pthread.h>
#include <stdio.h>

#define N_THREADS 10

typedef struct {
	 int arg1;
	 short arg2;
} thread_arg_t;

void *thread_function(void *args) {
  thread_arg_t *thread_args = (thread_arg_t *)args;
  printf("Hello from the thread, %d\n", thread_args->arg1);
  return NULL;
}

int main(int argc, char *argv[]) {
  pthread_t threads[N_THREADS];

  thread_arg_t args;

  int i = 0;
  for (; i < N_THREADS; i++) {
    args.arg1 = i;
    if (pthread_create(&threads[i], NULL, thread_function, (void *)&args)) {
      printf("Error create\n");
      return -1;
    }
  }

  for (i = 0; i < N_THREADS; i++)
    pthread_join(threads[i], NULL);
      
  return 0;
}

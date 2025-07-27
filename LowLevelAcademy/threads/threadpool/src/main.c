#include "threadpool.h"

void example_task(void* arg) {
    int* num = (int*)arg;
    printf("Processing task %d\n", *num);
    sleep(2);  /* Simulate task work */
    free(arg);  /* Free the allocated memory for the task argument */
}

int main() {
    threadpool_t pool;
    threadpool_init(&pool);

    /* Add tasks to the thread pool */
    int i = 0;
    for (; i < 100; i++) {
        int* task_num = malloc(sizeof(int));
        *task_num = i;
        threadpool_add_task(&pool, example_task, task_num);
    }

    /* Let tasks complete */
    sleep(40);

    threadpool_destroy(&pool);

    return 0;
}

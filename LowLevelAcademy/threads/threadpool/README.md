# Threadpool Example

This directory contains a minimal C threadpool implementation and example usage.

### Build

Run `make` in this directory to build the static library and the example program:

```sh
make
```

This will produce:
- `bin/libthreadpool.a` — the threadpool static library
- `bin/main` — the example executable

### Usage

Run the example program:

```sh
./bin/main
```

You should see output like:

```
Processing task 0
Processing task 2
Processing task 7
Processing task 3
Processing task 4
Processing task 5
Processing task 6
Processing task 1
Processing task 8
Processing task 9
Processing task 1
...
Processing task 99
```
Processed in chunks of 8

### How it works

- The threadpool is initialized with `threadpool_init()`
- Tasks are added with `threadpool_add_task()`
- Each task is processed by a worker thread
- The pool is destroyed with `threadpool_destroy()`

### Requirements

- GCC or compatible C compiler
- POSIX threads (pthreads)

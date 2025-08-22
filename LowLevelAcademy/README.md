# LOW LEVEL ACADEMY PROJECTS

A collection of C programming projects focused on systems programming and low-level concepts


## Projects

### [ARM Assembly](arm/README.md)
Learning ARMv7 assembly programming with direct Linux syscalls:
- Basic programs (hello world, loops, I/O)
- Interactive shell implementation (holmshell)
- Process management with fork/execve
- No C library dependencies - pure syscalls

### [Database System](database/README.md)
A custom binary database implementation with:
- CRUD operations for employee records
- Network-byte-order data storage
- Custom binary file format
- Command-line interface

### [Network Server](network/network/README.md)
A networked employee database system with client-server architecture:
- Poll-based server handling multiple simultaneous clients
- Custom binary protocol with state machine
- Persistent database storage
- Client application for database operations

### [Threadpool Example](threads/threadpool/README.md)
A minimal POSIX threadpool implementation:
- Static library and example program
- Task queue with worker threads
- Demonstrates thread synchronization and concurrency
- Simple Makefile for building and running

### [HTTP Server](httpserver/README.md)
A simple HTTP server implementation with JSON support:
- TCP socket-based HTTP/1.1 server
- JSON parsing using cJSON library
- GET and POST request handling
- Static file serving and form processing
- FreeBSD compatibility patches included

## Prerequisites

```bash
$ pacman -S gnu-netcat strace
```

These projects demonstrate foundational systems programming concepts including memory management, binary protocols, file I/O, networking, and multithreading.
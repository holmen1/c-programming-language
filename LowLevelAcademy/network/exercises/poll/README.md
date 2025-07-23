# TCP Echo Server with Poll

This example demonstrates a TCP echo server that can handle multiple client connections simultaneously using the `poll()` system call.

## What is Poll?

The `poll()` system call allows a program to monitor multiple file descriptors, waiting until one or more become "ready" for some I/O operation. In this server:

- `poll()` monitors both the listening socket and all client connections
- When new connections arrive, they're accepted and added to the monitored set
- When a client sends data, the server reads it and echoes it back

Poll offers advantages over select:
- No arbitrary file descriptor limit (select is limited by FD_SETSIZE)
- More efficient for large numbers of file descriptors
- Clearer separation between the file descriptors and the events of interest

## Understanding the strace Output
```bash
$ strace ./bin/server 
socket(AF_INET, SOCK_STREAM, IPPROTO_IP) = 3
setsockopt(3, SOL_SOCKET, SO_REUSEADDR, [1], 4) = 0
bind(3, {sa_family=AF_INET, sin_port=htons(5555), sin_addr=inet_addr("0.0.0.0")}, 16) = 0
listen(3, 5)                            = 0
fstat(1, {st_mode=S_IFCHR|0620, st_rdev=makedev(0x88, 0x3), ...}) = 0
getrandom("\xc2\x7a\x0c\x5e\xfb\x2d\xc1\xa1", 8, GRND_NONBLOCK) = 8
brk(NULL)                               = 0x5b39d16d6000
brk(0x5b39d16f7000)                     = 0x5b39d16f7000
write(1, "Server started on port 5555. Con"..., 61Server started on port 5555. Connect with: nc localhost 5555
) = 61
write(1, "Waiting for activity...\n", 24Waiting for activity...
) = 24
poll([{fd=3, events=POLLIN}], 1, -1)    = 1 ([{fd=3, revents=POLLIN}])
accept(3, {sa_family=AF_INET, sin_port=htons(43112), sin_addr=inet_addr("127.0.0.1")}, [16]) = 4
write(1, "New connection from 127.0.0.1:43"..., 51New connection from 127.0.0.1:43112 on socket fd 4
) = 51
sendto(4, "Welcome to the echo server! Type"..., 60, 0, NULL, 0) = 60
write(1, "Waiting for activity...\n", 24Waiting for activity...
) = 24
poll([{fd=3, events=POLLIN}, {fd=4, events=POLLIN}], 2, -1) = 1 ([{fd=4, revents=POLLIN}])
read(4, "hello\n", 1023)                = 6
write(1, "Received from fd 4: hello\n", 26Received from fd 4: hello
) = 26
sendto(4, "Server echoes: hello\n", 21, 0, NULL, 0) = 21
write(1, "Waiting for activity...\n", 24Waiting for activity...
) = 24
poll([{fd=3, events=POLLIN}, {fd=4, events=POLLIN}], 2, -1) = 1 ([{fd=3, revents=POLLIN}])
accept(3, {sa_family=AF_INET, sin_port=htons(38188), sin_addr=inet_addr("127.0.0.1")}, [16]) = 5
write(1, "New connection from 127.0.0.1:38"..., 51New connection from 127.0.0.1:38188 on socket fd 5
) = 51
sendto(5, "Welcome to the echo server! Type"..., 60, 0, NULL, 0) = 60
write(1, "Waiting for activity...\n", 24Waiting for activity...
) = 24
poll([{fd=3, events=POLLIN}, {fd=4, events=POLLIN}, {fd=5, events=POLLIN}], 3, -1) = 1 ([{fd=5, revents=POLLIN}])
read(5, "hello2\n", 1023)               = 7
write(1, "Received from fd 5: hello2\n", 27Received from fd 5: hello2
) = 27
sendto(5, "Server echoes: hello2\n", 22, 0, NULL, 0) = 22
write(1, "Waiting for activity...\n", 24Waiting for activity...
) = 24
poll([{fd=3, events=POLLIN}, {fd=4, events=POLLIN}, {fd=5, events=POLLIN}], 3, -1) = 1 ([{fd=4, revents=POLLIN}])
read(4, "hello\n", 1023)                = 6
write(1, "Received from fd 4: hello\n", 26Received from fd 4: hello
) = 26
sendto(4, "Server echoes: hello\n", 21, 0, NULL, 0) = 21
write(1, "Waiting for activity...\n", 24Waiting for activity...
) = 24
poll([{fd=3, events=POLLIN}, {fd=4, events=POLLIN}, {fd=5, events=POLLIN}], 3, -1
```

## Detailed strace Analysis

The strace output provides a detailed view of how the poll server handles multiple connections:

### 1. Initial Server Setup
```bash
socket(AF_INET, SOCK_STREAM, IPPROTO_IP) = 3
setsockopt(3, SOL_SOCKET, SO_REUSEADDR, [1], 4) = 0
bind(3, {sa_family=AF_INET, sin_port=htons(5555), sin_addr=inet_addr("0.0.0.0")}, 16) = 0
listen(3, 5)                            = 0
```
Server socket is created, configured, bound to port 5555, and set to listen mode.

### 2. First Poll Operation
```bash
poll([{fd=3, events=POLLIN}], 1, -1)    = 1 ([{fd=3, revents=POLLIN}])
```
Initially monitoring only the server socket (fd 3). Activity detected on server socket.

### 3. First Client Connection
```bash
accept(3, {sa_family=AF_INET, sin_port=htons(43112), sin_addr=inet_addr("127.0.0.1")}, [16]) = 4
sendto(4, "Welcome to the echo server! Type"..., 60, 0, NULL, 0) = 60
```
First client connects from port 43112 and is assigned fd 4. Welcome message sent.

### 4. Updated Poll Array
```bash
poll([{fd=3, events=POLLIN}, {fd=4, events=POLLIN}], 2, -1) = 1 ([{fd=4, revents=POLLIN}])
```
Now monitoring both server (fd 3) and first client (fd 4). Activity detected on client socket.

### 5. Data from First Client
```bash
read(4, "hello\n", 1023)                = 6
sendto(4, "Server echoes: hello\n", 21, 0, NULL, 0) = 21
```
Received "hello" from first client and echoed it back.

### 6. Second Client Connection
```bash
poll([{fd=3, events=POLLIN}, {fd=4, events=POLLIN}], 2, -1) = 1 ([{fd=3, revents=POLLIN}])
accept(3, {sa_family=AF_INET, sin_port=htons(38188), sin_addr=inet_addr("127.0.0.1")}, [16]) = 5
sendto(5, "Welcome to the echo server! Type"..., 60, 0, NULL, 0) = 60
```
Second client connects from port 38188 and is assigned fd 5. Welcome message sent.

### 7. Three Connections Monitored
```bash
poll([{fd=3, events=POLLIN}, {fd=4, events=POLLIN}, {fd=5, events=POLLIN}], 3, -1) = 1 ([{fd=5, revents=POLLIN}])
```
Now monitoring server and both clients. Activity detected on second client.

### 8. Concurrent Client Handling
```bash
read(5, "hello2\n", 1023)               = 7
sendto(5, "Server echoes: hello2\n", 22, 0, NULL, 0) = 22

poll([{fd=3, events=POLLIN}, {fd=4, events=POLLIN}, {fd=5, events=POLLIN}], 3, -1) = 1 ([{fd=4, revents=POLLIN}])
read(4, "hello\n", 1023)                = 6
sendto(4, "Server echoes: hello\n", 21, 0, NULL, 0) = 21
```
The server handles both clients without blocking either one, demonstrating the power of the poll() approach for concurrent connections.
```

This explanation shows exactly how the poll mechanism works, highlighting how the server dynamically expands its monitoring set as new clients connect, and how it handles multiple clients concurrently.This explanation shows exactly how the poll mechanism works, highlighting how the server dynamically expands its monitoring set as new clients connect, and how it handles multiple clients concurrently.


```bash
$ gcc -o /bin/server -std=c90 -Wall server.c
$ strace ./bin/server
```

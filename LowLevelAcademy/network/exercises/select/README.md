# TCP Echo Server with Select

This example demonstrates a TCP echo server that can handle multiple client connections simultaneously using the `select()` system call.

## What is Select?

The `select()` system call allows a program to monitor multiple file descriptors, waiting until one or more become "ready" for some I/O operation. In this server:

- `select()` monitors both the listening socket and all client connections
- When new connections arrive, they're accepted and added to the monitored set
- When a client sends data, the server reads it and echoes it back

## Understanding the strace Output

The strace output shows the sequence of system calls made by the server:

1. **Initial setup**:
   - `socket()` creates a TCP socket
   - `setsockopt()` sets socket options (SO_REUSEADDR allows reusing the port)
   - `bind()` assigns the socket to port 5555
   - `listen()` marks the socket as passive, ready to accept connections

```bash
socket(AF_INET, SOCK_STREAM, IPPROTO_IP) = 3
setsockopt(3, SOL_SOCKET, SO_REUSEADDR, [1], 4) = 0
bind(3, {sa_family=AF_INET, sin_port=htons(5555), sin_addr=inet_addr("0.0.0.0")}, 16) = 0
listen(3, 5)                            = 0
```

2. **Main loop with select**:
   - `pselect6()` waits for activity on monitored file descriptors
   - When the listening socket (fd 3) shows activity, a new connection is accepted
   - When a client socket shows activity, data is read and echoed back

```bash
write(1, "Waiting for activity...\n", 24Waiting for activity...) = 24
pselect6(4, [3], NULL, NULL, NULL, NULL
```

3. **Multiple clients**:
   - First client connects on fd 4, sends "hello"
   - Second client connects on fd 5, sends "hello2"
   - Both are handled independently without blocking each other

```bash
$ nc localhost 5555
Welcome to the echo server! Type something and press enter.
hello
Server echoes: hello
```

```bash
$ nc localhost 5555
Welcome to the echo server! Type something and press enter.
hello
Server echoes: hello2
```

```bash
pselect6(4, [3], NULL, NULL, NULL, NULL) = 1 (in [3])
accept(3, {sa_family=AF_INET, sin_port=htons(59936), sin_addr=inet_addr("127.0.0.1")}, [16]) = 4
write(1, "New connection from 127.0.0.1:59"..., 51New connection from 127.0.0.1:59936 on socket fd 4
) = 51
sendto(4, "Welcome to the echo server! Type"..., 60, 0, NULL, 0) = 60
write(1, "Waiting for activity...\n", 24Waiting for activity...
) = 24
pselect6(5, [3 4], NULL, NULL, NULL, NULL) = 1 (in [4])
read(4, "hello\n", 1023)                = 6
write(1, "Received from fd 4: hello\n", 26Received from fd 4: hello
) = 26
sendto(4, "Server echoes: hello\n", 21, 0, NULL, 0) = 21
write(1, "Waiting for activity...\n", 24Waiting for activity...
) = 24
pselect6(5, [3 4], NULL, NULL, NULL, NULL) = 1 (in [3])
accept(3, {sa_family=AF_INET, sin_port=htons(48502), sin_addr=inet_addr("127.0.0.1")}, [16]) = 5
write(1, "New connection from 127.0.0.1:48"..., 51New connection from 127.0.0.1:48502 on socket fd 5
) = 51
sendto(5, "Welcome to the echo server! Type"..., 60, 0, NULL, 0) = 60
write(1, "Waiting for activity...\n", 24Waiting for activity...
) = 24
pselect6(6, [3 4 5], NULL, NULL, NULL, NULL) = 1 (in [5])
read(5, "hello2\n", 1023)               = 7
write(1, "Received from fd 5: hello2\n", 27Received from fd 5: hello2
) = 27
sendto(5, "Server echoes: hello2\n", 22, 0, NULL, 0) = 22
write(1, "Waiting for activity...\n", 24Waiting for activity...
) = 24
pselect6(6, [3 4 5], NULL, NULL, NULL, NULL
```

This approach is more efficient than creating a new thread or process for each connection, making it suitable for handling many concurrent connections with minimal resources.

```bash
$ gcc -o /bin/server -std=c90 -Wall server.c
$ strace ./bin/server
```

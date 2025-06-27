```bash
$ gcc -o server -std=c90 -Wall server.c 
$ gcc -o client -std=c90 -Wall client.c 
$ strace ./server
```

```bash
$ ./client 127.0.0.1
Successfully connected to the server, protocol v1
```

server strace:
```bash
socket(AF_INET, SOCK_STREAM, IPPROTO_IP) = 3
bind(3, {sa_family=AF_INET, sin_port=htons(5555), sin_addr=inet_addr("0.0.0.0")}, 16) = 0
listen(3, 5)                            = 0
accept(3, {sa_family=AF_INET, sin_port=htons(40930), sin_addr=inet_addr("127.0.0.1")}, [16]) = 4
write(4, "\0\0\0\0\0\4\0\0\0\0\0\1", 12) = 12
close(4)                                = 0
accept(3, 
```
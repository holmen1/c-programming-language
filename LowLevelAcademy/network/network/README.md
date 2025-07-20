# Network Server Project - Makefile and Testing

This document describes the build system and testing infrastructure for the network server project.

## Project Build System

The project uses a dual-Makefile approach for clarity and separation of concerns:

- Makefile - Main build system for compiling and running the server
- Makefile.test - Dedicated testing framework for verifying server functionality

## Main Makefile

### Key Targets

| Target | Description |
|--------|-------------|
| `make` | Default target that builds the server |
| `make debug` | Build with debug symbols and no optimization |
| `make clean` | Remove compiled files and databases |

### Usage Examples

```bash
# Run server, create new database
./dbserver -n -f my.db -p 8080
```

```bash
# Run client, add employee
./dbcli -h 127.0.0.1 -p 8080 -a "Mats,46 Hallandsgatan,200"
```

## Testing Framework

The Makefile.test provides isolated tests for critical server functionality.

### Key Test Targets

| Target | Description |
|--------|-------------|
| `test-socket` | Verifies the server can bind to a port and accept connections |
| `test-shutdown` | Tests if the server can terminate gracefully on signal |
| `test` | Runs all tests in sequence |

### Running Tests

```bash
# Run all tests
make -f Makefile.test

# Run a specific test
make -f Makefile.test test-socket
```

## How the Tests Work

Each test follows a common pattern:

1. Start the server with specific parameters
2. Perform an action (connect, send data, etc.)
3. Verify the expected behavior
4. Clean up (terminate server, remove temporary files)

## Extending the Tests

To add a new test:

1. Create a new target in Makefile.test
2. Follow the pattern of existing tests
3. Add your test to the `test-all` dependency list

Example for adding a data test:

```makefile
# Test data handling
test-data:
	@echo "\n==== Testing data handling ===="
	@./$(TARGET_SRV) -f ./test.db -n -p 9092 & \
	SERVER_PID=$$!; \
	sleep 1; \
	echo "Sending test data..."; \
	echo "TEST_DATA" | nc localhost 9092; \
	# Add verification steps here
	kill -TERM $$SERVER_PID;
```

## Debugging Tips

- Use `make debug` to build with debugging symbols
- Set up VS Code debugging with the provided launch.json
- Add `sleep` commands in tests for easier manual debugging
- Redirect server output to a log file for inspection

## Best Practices

1. Always run `make clean` before building for production
2. Run the test suite before committing changes
3. Separate building from running for better control
4. Use `.PHONY` for non-file targets in Makefiles


$ make run-server
Makefile:72: warning: overriding recipe for target 'run-server'
Makefile:55: warning: ignoring old recipe for target 'run-server'
Cleaning up any running servers...
killall -9 dbserver 2>/dev/null || true
rm -f obj/srv/*.o obj/cli/*.o
rm -f bin/*
rm -f *.db
gcc -std=c90 -Wall -c src/srv/file.c -o obj/srv/file.o -Iinclude
gcc -std=c90 -Wall -c src/srv/main.c -o obj/srv/main.o -Iinclude
src/srv/main.c: In function ‘main’:
src/srv/main.c:32:14: warning: variable ‘newfile’ set but not used [-Wunused-but-set-variable]
   32 |         bool newfile = false;
      |              ^~~~~~~
gcc -std=c90 -Wall -c src/srv/parse.c -o obj/srv/parse.o -Iinclude
src/srv/parse.c: In function ‘output_file’:
src/srv/parse.c:155:5: warning: implicit declaration of function ‘ftruncate’; did you mean ‘strncat’? [-Wimplicit-function-declaration]
  155 |     ftruncate(fd, sizeof(struct dbheader_t) + sizeof(struct employee_t) * realcount);
      |     ^~~~~~~~~
      |     strncat
gcc -std=c90 -Wall -c src/srv/srvpoll.c -o obj/srv/srvpoll.o -Iinclude
gcc -std=c90 -Wall -o bin/dbserver obj/srv/file.o obj/srv/main.o obj/srv/parse.o obj/srv/srvpoll.o
Starting server on port 8080...
./bin/dbserver -f ./mynewdb.db -n -p 8080 & \
SERVER_PID=$!; \
echo "Server running with PID: $SERVER_PID"; \
echo "Press Ctrl+C to stop the server"; \
trap "echo 'Stopping server...'; kill -TERM $SERVER_PID; sleep 2; kill -9 $SERVER_PID 2>/dev/null || true; exit 0" INT TERM; \
while kill -0 $SERVER_PID 2>/dev/null; do sleep 1; done
Server running with PID: 36326
Press Ctrl+C to stop the server
Server started on port 8080. Connect with: nc localhost 8080
New connection from 127.0.0.1:54728
Client connected in slot 0 with fd 4
Received from client 0: hello

Client in slot 0 disconnected

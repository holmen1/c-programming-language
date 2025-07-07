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
| `make server-only` | Build the server without running it |
| `make run` | Build and run the server on port 8080 |
| `make debug` | Build with debug symbols and no optimization |
| `make clean` | Remove compiled files and databases |

### Usage Examples

```bash
# Just build the server
make

# Build and run the server on port 8080
make run

# Build with debugging symbols
make debug

# Clean up all generated files
make clean
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

TARGET_SRV = bin/dbserver
TARGET_CLI = bin/dbcli

CFLAGS = -std=c90 -Wall

SRC_SRV = $(wildcard src/srv/*.c)
OBJ_SRV = $(SRC_SRV:src/srv/%.c=obj/srv/%.o)

SRC_CLI = $(wildcard src/cli/*.c)
OBJ_CLI = $(SRC_CLI:src/cli/%.c=obj/cli/%.o)

# Extract database-related files from server source
SRC_DB = src/srv/file.c src/srv/parse.c
OBJ_DB = $(SRC_DB:src/srv/%.c=obj/srv/%.o)

.PHONY: default clean server-only run test-server debug directories cleanup

default: $(TARGET_SRV) $(TARGET_CLI)

# Build targets
server-only: $(TARGET_SRV)

$(TARGET_SRV): $(OBJ_SRV)
	gcc $(CFLAGS) -o $@ $(OBJ_SRV)

$(OBJ_SRV): obj/srv/%.o: src/srv/%.c
	gcc $(CFLAGS) -c $< -o $@ -Iinclude

$(TARGET_CLI): $(OBJ_CLI) $(OBJ_DB)
	gcc $(CFLAGS) -o $@ $(OBJ_CLI) $(OBJ_DB)

$(OBJ_CLI): obj/cli/%.o: src/cli/%.c
	gcc $(CFLAGS) -c $< -o $@ -Iinclude

# Debug build
debug: CFLAGS += -g -O0
debug: clean server-only

# Directory creation
directories:
	mkdir -p bin obj/srv obj/cli

# Cleanup targets
cleanup:
	@echo "Cleaning up any running servers..."
	killall -9 dbserver 2>/dev/null || true

clean: cleanup
	rm -f obj/srv/*.o obj/cli/*.o
	rm -f bin/*
	rm -f *.db

# Run server
run-server: clean server-only
	@echo "Starting server on port 8080..."
	./$(TARGET_SRV) -f ./mynewdb.db -n -p 8080 & \
	SERVER_PID=$$!; \
	echo "Server running with PID: $$SERVER_PID"; \
	echo "Press Ctrl+C to stop the server"; \
	trap "echo 'Stopping server...'; kill -TERM $$SERVER_PID; sleep 2; kill -9 $$SERVER_PID 2>/dev/null || true; exit 0" INT TERM; \
	while kill -0 $$SERVER_PID 2>/dev/null; do sleep 1; done

# Just build everything without running client
build-all: clean default
	@echo "Server and client successfully built"
	@echo "Server binary: $(TARGET_SRV)"
	@echo "Client binary: $(TARGET_CLI)"
	@echo "Database files included in client: $(SRC_DB)"

# Run server only (existing target remains unchanged)
run-server: clean server-only
	@echo "Starting server on port 8080..."
	./$(TARGET_SRV) -f ./mynewdb.db -n -p 8080 & \
	SERVER_PID=$$!; \
	echo "Server running with PID: $$SERVER_PID"; \
	echo "Press Ctrl+C to stop the server"; \
	trap "echo 'Stopping server...'; kill -TERM $$SERVER_PID; sleep 2; kill -9 $$SERVER_PID 2>/dev/null || true; exit 0" INT TERM; \
	while kill -0 $$SERVER_PID 2>/dev/null; do sleep 1; done

run: clean default
	@echo "Starting server on port 8080..."
	./$(TARGET_SRV) -f ./mynewdb.db -n -p 8080 & \
	SERVER_PID=$$!; \
	echo "Server running with PID: $$SERVER_PID"; \
	sleep 2; \
	echo "Starting client..."; \
	./$(TARGET_CLI)  -f ./mynewdb.db -l; \
	CLIENT_STATUS=$$?; \
	echo "Client finished with status $$CLIENT_STATUS, stopping server..."; \
	kill -TERM $$SERVER_PID; \
	sleep 1; \
	exit $$CLIENT_STATUS


# Updated help target to include the new build-all option
help:
	@echo "Available targets:"
	@echo "  make            - Build server and client"
	@echo "  make build-all  - Build everything and verify (no execution)"
	@echo "  make run-server - Run server only"
	@echo "  make run        - Run both server and client"
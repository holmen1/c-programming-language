TARGET_SRV = bin/dbserver

# Add C90 compilation flags
CFLAGS = -std=c90 -Wall

SRC_SRV = $(wildcard src/srv/*.c)
OBJ_SRV = $(SRC_SRV:src/srv/%.c=obj/srv/%.o)

.PHONY: default clean server-only run test-server directories cleanup

default: server-only

# Just build and run the server
run: clean server-only
	@echo "Killing any existing servers..."
	killall -9 dbserver 2>/dev/null || true
	@echo "Starting server on port 8080..."
	./$(TARGET_SRV) -f ./mynewdb.db -n -p 8080 & \
	SERVER_PID=$$!; \
	echo "Server running with PID: $$SERVER_PID"; \
	echo "Press Ctrl+C to stop the server"; \
	trap "echo 'Stopping server...'; kill -TERM $$SERVER_PID; sleep 2; kill -9 $$SERVER_PID 2>/dev/null || true; exit 0" INT TERM; \
	while kill -0 $$SERVER_PID 2>/dev/null; do sleep 1; done

# Quick test to verify server starts up correctly
test-server: server-only
	@echo "Killing any existing servers..."
	killall -9 dbserver 2>/dev/null || true
	@echo "Starting server on port 8181..."
	./$(TARGET_SRV) -f ./test.db -n -p 8181 & \
	sleep 1; \
	echo "Checking server status..."; \
	ps aux | grep dbserver | grep -v grep; \
	kill -9 $$(pidof dbserver) 2>/dev/null || true; \
	echo "Server test complete"

# Only build the server
server-only: $(TARGET_SRV)

cleanup:
	@echo "Cleaning up any running servers..."
	killall -9 dbserver 2>/dev/null || true

clean: cleanup
	rm -f obj/srv/*.o
	rm -f bin/dbserver
	rm -f *.db

# Create directories if they don't exist
directories:
	mkdir -p bin obj/srv

$(TARGET_SRV): directories $(OBJ_SRV)
	gcc $(CFLAGS) -o $@ $(OBJ_SRV)

$(OBJ_SRV): obj/srv/%.o: src/srv/%.c
	gcc $(CFLAGS) -c $< -o $@ -Iinclude
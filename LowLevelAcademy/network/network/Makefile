TARGET_SRV = bin/dbserver
CFLAGS = -std=c90 -Wall

SRC_SRV = $(wildcard src/srv/*.c)
OBJ_SRV = $(SRC_SRV:src/srv/%.c=obj/srv/%.o)

.PHONY: default clean server-only run test-server debug directories cleanup

default: server-only

# Build targets
server-only: $(TARGET_SRV)

$(TARGET_SRV): directories $(OBJ_SRV)
	gcc $(CFLAGS) -o $@ $(OBJ_SRV)

$(OBJ_SRV): obj/srv/%.o: src/srv/%.c
	gcc $(CFLAGS) -c $< -o $@ -Iinclude

# Debug build
debug: CFLAGS += -g -O0
debug: clean server-only

# Directory creation
directories:
	mkdir -p bin obj/srv

# Cleanup targets
cleanup:
	@echo "Cleaning up any running servers..."
	killall -9 dbserver 2>/dev/null || true

clean: cleanup
	rm -f obj/srv/*.o
	rm -f bin/dbserver
	rm -f *.db

# Run target
run: clean server-only
	@echo "Starting server on port 8080..."
	./$(TARGET_SRV) -f ./mynewdb.db -n -p 8080 & \
	SERVER_PID=$$!; \
	echo "Server running with PID: $$SERVER_PID"; \
	echo "Press Ctrl+C to stop the server"; \
	trap "echo 'Stopping server...'; kill -TERM $$SERVER_PID; sleep 2; kill -9 $$SERVER_PID 2>/dev/null || true; exit 0" INT TERM; \
	while kill -0 $$SERVER_PID 2>/dev/null; do sleep 1; done
TARGET_SRV = bin/dbserver
TARGET_CLI = bin/dbcli

CFLAGS = -std=c90 -Wall

SRC_SRV = $(wildcard src/srv/*.c)
OBJ_SRV = $(SRC_SRV:src/srv/%.c=obj/srv/%.o)

SRC_CLI = $(wildcard src/cli/*.c)
OBJ_CLI = $(SRC_CLI:src/cli/%.c=obj/cli/%.o)

.PHONY: all clean directories

all: directories $(TARGET_SRV) $(TARGET_CLI)

# Build server
$(TARGET_SRV): $(OBJ_SRV)
	gcc $(CFLAGS) -o $@ $(OBJ_SRV)

$(OBJ_SRV): obj/srv/%.o: src/srv/%.c
	gcc $(CFLAGS) -c $< -o $@ -Iinclude

# Build the client
$(TARGET_CLI): $(OBJ_CLI)
	gcc $(CFLAGS) -o $@ $(OBJ_CLI)

$(OBJ_CLI): obj/cli/%.o: src/cli/%.c
	gcc $(CFLAGS) -c $< -o $@ -Iinclude

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

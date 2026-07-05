# Compiler to use
CC = gcc

# Compiler flags
CFLAGS = -std=gnu90 -Wall -Wextra

# Name of the output executable
TARGET = test

# Source files for main
SRCS = strcat.c test.c

# Default target to build the executable
all:
	$(CC) $(SRCS) -o $(TARGET)

# Rule to clean up build files (includes test files)
clean:
	rm -f $(TARGET)

.PHONY: all clean

# Compiler to use
CC = gcc

# Compiler flags
CFLAGS = -std=gnu90 -Wall -Wextra

# Name of the output executable
TARGET = mysort

# Source files for main
SRCS = main.c readlines.c alloc.c qsort.c

# Default target to build the executable
all:
	$(CC) $(CFLAGS) $(SRCS) -o $(TARGET)

# Rule to clean up build files (includes test files)
clean:
	rm -f $(TARGET)

.PHONY: all clean

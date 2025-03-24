# Compiler to use
CC = gcc

# Compiler flags
CFLAGS = -std=c90 -Wall

# Linker flags
LDFLAGS = -lm

# Name of the output executable
TARGET = calculator

# Source files
SRCS = getch.c getop.c stack.c main.c

# Object files (derived from source files)
OBJS = getch.o getop.o stack.o main.o

# Default target to build the executable
all: $(TARGET)

# Rule to link object files into the executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LDFLAGS)

# Rule to compile source files into object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Rule to clean up build files
clean:
	rm -f $(OBJS) $(TARGET)

# Declare targets that are not actual files
.PHONY: all clean
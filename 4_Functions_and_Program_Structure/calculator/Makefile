# Compiler to use
CC = gcc

# Compiler flags
CFLAGS      = -std=c90 -Wall
TEST_CFLAGS = -std=gnu11 -Wall

# Linker flags
LDFLAGS = -lm

# Name of the output executable
TARGET = calculator

# Source files for main
SRCS = stack.c getch.c getop.c main.c

# Object files (now in dependency order)
OBJS = stack.o getch.o getop.o main.o

# Test executable name
TEST_TARGET = test_calculator

# Test sources (shared modules + test driver)
TEST_SRCS = stack.c getch.c getop.c test_calculator.c

# Default target to build the executable
all: $(TARGET)

# Rule to link object files into the executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LDFLAGS)

# Rule to compile source files into object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Build test binary (depends on calculator for integration tests)
$(TEST_TARGET): $(TARGET) $(TEST_SRCS)
	$(CC) $(TEST_CFLAGS) -o $(TEST_TARGET) $(TEST_SRCS) $(LDFLAGS)

# Phony target to run tests (builds and executes)
test: $(TEST_TARGET)
	./$(TEST_TARGET) 2>/dev/null

# Rule to clean up build files (includes test files)
clean:
	rm -f $(OBJS) $(TARGET) $(TEST_TARGET)

.PHONY: all clean test

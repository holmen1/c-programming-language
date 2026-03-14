# Compiler to use
CC = gcc

# Compiler flags
CFLAGS = -std=c90 -Wall

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

# Source files for tests (includes shared sources, reordered)
TEST_SRCS = stack.c getch.c getop.c test_calculator.c

# Object files for tests
TEST_OBJS = stack.o getch.o getop.o test_calculator.o

# Default target to build the executable
all: $(TARGET)

# Rule to link object files into the executable
$(TARGET): $(OBJS)
	$(CC) $(CFLAGS) -o $(TARGET) $(OBJS) $(LDFLAGS) 

# Rule to compile source files into object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Rule to build the test executable
$(TEST_TARGET): $(TEST_OBJS)
	$(CC) $(CFLAGS) -o $(TEST_TARGET) $(TEST_OBJS) $(LDFLAGS)

# Phony target to run tests (builds and executes)
test: $(TEST_TARGET)
	./$(TEST_TARGET)

# Rule to clean up build files (includes test files)
clean:
	rm -f $(OBJS) $(TARGET) $(TEST_OBJS) $(TEST_TARGET)

.PHONY: all clean test


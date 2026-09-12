CC = gcc

CFLAGS = -std=c90 -Wall -Wextra

SRCS = getword.c main.c

OBJS = getword.o main.o

# Rule to link object files into the executable
keywords: $(OBJS)
	$(CC) $(CFLAGS) -o keywords $(OBJS)

# Rule to compile source files into object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Rule to clean up build files (includes test files)
clean:
	rm -f $(OBJS) keywords

.PHONY:
	clean

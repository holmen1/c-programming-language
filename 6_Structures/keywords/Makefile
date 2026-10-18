CC = gcc

CFLAGS = -std=c90 -Wall -Wextra

SRCS = getword.c main.c

OBJS = getword.o main.o

OBJS2 = getword.o main2.o

# Rule to link object files into the executable
keywords: $(OBJS)
	$(CC) $(CFLAGS) -o keywords $(OBJS)

keywords2: $(OBJS2)
	$(CC) $(CFLAGS) -o keywords2 $(OBJS2)

# Rule to compile source files into object files
%.o: %.c
	$(CC) $(CFLAGS) -c $< -o $@

# Rule to clean up build files (includes test files)
clean:
	rm -f $(OBJS) main2.o keywords*

.PHONY:
	clean

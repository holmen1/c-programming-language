

CC = gcc
CFLAGS = -std=c90 -Wall -Wextra -g -Iinclude
LDFLAGS = -pthread

SRCDIR = src
OBJDIR = obj
BINDIR = bin

SOURCES = $(wildcard $(SRCDIR)/*.c)
OBJECTS = $(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(filter-out $(SRCDIR)/main.c,$(SOURCES)))
MAIN_OBJ = $(OBJDIR)/main.o

all: $(BINDIR)/libthreadpool.a $(BINDIR)/main

$(BINDIR)/libthreadpool.a: $(OBJECTS) | $(BINDIR)
	ar rcs $@ $^

$(BINDIR)/main: $(MAIN_OBJ) $(OBJECTS) | $(BINDIR)
	$(CC) $(CFLAGS) $^ -o $@ $(LDFLAGS)

$(OBJDIR)/%.o: $(SRCDIR)/%.c | $(OBJDIR)
	$(CC) $(CFLAGS) -c $< -o $@

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(BINDIR):
	mkdir -p $(BINDIR)

clean:
	rm -f $(OBJDIR)/*.o $(BINDIR)/*

.PHONY: all clean


CC = gcc
CFLAGS = -std=c90 -Wall -Wextra -Iinclude

SRCDIR = src
OBJDIR = obj
BINDIR = bin

TARGET = $(BINDIR)/httpserver

SOURCES = $(wildcard $(SRCDIR)/*.c)
OBJECTS = $(patsubst $(SRCDIR)/%.c,$(OBJDIR)/%.o,$(SOURCES))

all: $(TARGET)

$(TARGET): $(OBJECTS) | $(BINDIR)
	$(CC) $(CFLAGS) $^ -o $@

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


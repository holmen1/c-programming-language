#include <stdio.h>
#include <stdlib.h>

#define TABN 8
#define MAXLINELEN 100

static void entab_line(char *dst, const char *src, int tab_width) {
  int c;
  int nspaces = 0;
  int col = 1;
  while ((c = *src++) != '\n') {
    if (c == ' ') {
      nspaces++;
      if (col % tab_width == 0) {
        *dst++ = '\t';
        nspaces = 0;
      }
      col++;
      continue;
    }
    while (nspaces) {
      *dst++ = ' ';
      nspaces--;
    }
    *dst++ = c;
    col++;
  }
  *dst++ = '\n';
  *dst = '\0';
}

int main(int argc, char *argv[]) {
  char *buffer = NULL;
  size_t bufsize = 0; // Size of the buffer
  ssize_t nread;      // Number of characters read
  char out[MAXLINELEN];

  while ((nread = getline(&buffer, &bufsize, stdin)) != -1) {
    if (nread > MAXLINELEN) {
      free(buffer);
      return 1;
    }

    entab_line(out, buffer, TABN);
    fputs(out, stdout);
  }
  free(buffer);
  return 0;
}

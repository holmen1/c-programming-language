#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define max(A, B) ((A) > (B) ? (A) : (B))
#define MAXLINES 1000
static char *lineptr[MAXLINES];
static char buf[MAXLINES * 120];

int readlines_to_buffer(FILE *s) {
  char *bufp = buf;
  char *line = NULL;
  size_t size = 0;
  ssize_t nread;
  ssize_t lread = 0;

  while ((nread = getline(&line, &size, s)) != -1) {
    if (lread < MAXLINES) {
      line[nread - 1] = '\0';
      strcpy(bufp, line);
      lineptr[lread++] = bufp;
      bufp += nread;
    } else {
      fprintf(stderr, "Read: %zd lines\n", lread);
      lread = -1;
      break;
    }
  }
  free(line);

  return lread;
}

int main(int argc, char *argv[]) {
  FILE *stream;
  int nlines = 10;

  if (argc == 3) {
    nlines = atoi(++argv[1]);
    stream = fopen(argv[2], "r");
  } else
    stream = fopen(argv[1], "r");
  if (stream == NULL) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }

  int lines = readlines_to_buffer(stream);
  fclose(stream);
  int tail = max(0, lines - nlines);
  while (tail < lines)
    printf("%s\n", lineptr[tail++]);

  return 0;
}

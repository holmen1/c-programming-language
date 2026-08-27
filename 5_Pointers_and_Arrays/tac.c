#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXLINES 1000
static char *lineptr[MAXLINES];
static char buf[MAXLINES * 120];

int tac(FILE *s) {
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

  if (argc != 2) {
    fprintf(stderr, "Usage: %s <file>\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  stream = fopen(argv[1], "r");
  if (stream == NULL) {
    perror("fopen");
    exit(EXIT_FAILURE);
  }

  int lines = tac(stream);
  fclose(stream);
  if (lines >= 0) {
    while (lines)
      printf("%s\n", lineptr[--lines]);
    exit(EXIT_SUCCESS);
  } else
    exit(EXIT_FAILURE);
}

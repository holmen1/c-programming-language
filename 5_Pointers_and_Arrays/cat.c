#include <stdio.h>
#include <stdlib.h>

void tac(FILE *s) {
  char *line = NULL;
  size_t size = 0;
  ssize_t nread;

  while ((nread = getline(&line, &size, s)) != -1)
    fwrite(line, nread, 1, stdout);
  free(line);
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
  tac(stream);

  fclose(stream);
  exit(EXIT_SUCCESS);
}

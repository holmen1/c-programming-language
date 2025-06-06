#include "readlines.h"
#include <stdio.h>
#include <stdlib.h>

#define MAXLINES 5000
#define DEFAULT_N 10

char *lineptr[MAXLINES];

int main(int argc, char *argv[]) {
  int nlines;
  int n_to_print = DEFAULT_N;
  char *filename = NULL;

  if (argc == 1) {
    fprintf(stderr, "Usage: %s [-n] filename\n", argv[0]);
    return 1;
  } else if (**++argv == '-') {
    n_to_print = atoi(++*argv);
    filename = *++argv;
  } else
    filename = *argv;

  if (filename != NULL) {
    if (freopen(filename, "r", stdin) == NULL) {
      fprintf(stderr, "Error: Can't open file %s\n", filename);
      return 1;
    }
  }

  if ((nlines = readlines(lineptr, MAXLINES)) >= 0) {

    if (nlines <= n_to_print)
      writelines(lineptr, nlines);
    else
      writelines(lineptr + nlines - n_to_print, n_to_print);

    return 0;
  } else {
    printf("error: input too big or could not read lines\n");
    return 1;
  }
}

/*
$ gcc -std=c90 -Wall -o tail main.c readlines.c alloc.c
*/

#include <stdio.h>
#include "readlines.h"

#define MAXLINES 5000
#define DEFAULT_N 10

char* lineptr[MAXLINES];

int main(int argc, char *argv[])
{
  int nlines;
  int n_to_print = DEFAULT_N;
  char *filename = NULL;

  if (argc == 1) {
      fprintf(stderr, "Usage: %s filename\n", argv[0]);
      return 1;
  } else if (argc == 2) {
      filename = argv[1];
  } else {
      fprintf(stderr, "Usage: %s filename\n", argv[0]);
      return 1;
  }

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

*/

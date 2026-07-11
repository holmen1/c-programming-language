#include "qsort.h"
#include <stdio.h>

#define MAXLINES 5000
#define MAXSTORAGE 1000 * MAXLINES

int readlines(char *lineptr[], int maxlines, char *linestorage, int maxstorage);
void writelines(char *lineptr[], int maxlines);

int main() {
  int nlines;
  char *lineptr[MAXLINES];
  char linestorage[MAXSTORAGE];

  if ((nlines = readlines(lineptr, MAXLINES, linestorage, MAXSTORAGE)) >= 0) {
    qsort(lineptr, 0, nlines - 1);
    writelines(lineptr, nlines);
    return 0;
  } else {
    printf("error: input too big to sort\n");
    return 1;
  }
}

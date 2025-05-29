#include <stdio.h>
#include "string.h"
#include "alloc.h"

#define MAXLINES 5000

char* lineptr[MAXLINES];

int main()
{
  /* int nlines; */
  lineptr[0] = alloc(10);
  lineptr[1] = alloc(10);
  getline(lineptr[0], 10);
  copy(lineptr[1], lineptr[0]);
  printf("%s\n", lineptr[0]);
  printf("%s\n", lineptr[1]);

  return 0;
}

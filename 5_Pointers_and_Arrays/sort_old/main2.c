#include <stdio.h>
#include <time.h>
#include "readlines2.h"
#include "qsort.h"

#define MAXLINES 5000
#define MAXSTOR 100000

char* lineptr[MAXLINES];

int main()
{
  int nlines;
  char linestorage[MAXSTOR];

  clock_t start_time, end_time;
  double read_time, sort_time;

  start_time = clock();
  if ((nlines = readlines2(lineptr, MAXLINES, linestorage, MAXSTOR)) >= 0) {
    end_time = clock();
    read_time = ((double) (end_time - start_time)) / CLOCKS_PER_SEC;

    start_time = clock();
    qsort(lineptr, 0, nlines-1);
    end_time = clock();
    sort_time = ((double) (end_time - start_time)) / CLOCKS_PER_SEC;

    writelines(lineptr, nlines);

    printf("Time taken by readlines: %f seconds\n", read_time);
    printf("Time taken by qsort: %f seconds\n", sort_time);
    return 0;
  } else {
    printf("error: input too big to sort\n");
    return 1;
  }
}

/*
Time taken by readlines: 0.000744 seconds
Time taken by qsort: 0.001277 seconds
*/

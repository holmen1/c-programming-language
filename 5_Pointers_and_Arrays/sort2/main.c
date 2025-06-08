#include "numcmp.h"
#include "readlines.h"
#include <stdio.h>
#include <string.h>

#define MAXLINES 5000
char *lineptr[MAXLINES];

void qsort(void *lineptr[], int left, int right, int (*comp)(void *, void *));
int numcmp(char *, char *);
void reverse_lines(void *lines[], int num_lines);

int main(int argc, char *argv[]) {
  int nlines;
  int numeric = 0;

  if (argc > 1 && strcmp(argv[1], "-n") == 0)
    numeric = 1;
  if ((nlines = readlines(lineptr, MAXLINES)) >= 0) {
    qsort((void **)lineptr, 0, nlines - 1,
          (int (*)(void *, void *))(numeric ? (int (*)(void *, void *))numcmp
                                            : (int (*)(void *, void *))strcmp));
    reverse_lines((void **)lineptr, nlines);
    writelines(lineptr, nlines);
    return 0;
  } else {
    printf("error: input too big to sort\n");
    return 1;
  }
}

void qsort(void *v[], int left, int right, int (*comp)(void *, void*))
{
    int i, last;
    void swap(void *v[], int i, int j);

    if (left >= right)
        return;
    swap(v, left, (left + right)/2);
    last = left;    /* move partition elem to v[0] */
    for (i = left + 1; i <= right; i++)
        if ((*comp)(v[i], v[left]) < 0)
            swap(v, ++last, i);
    swap(v, left, last);
    qsort(v, left, last-1, comp);
    qsort(v, last+1, right, comp);
}


void swap(void *v[], int i, int j)
{
    void *tmp;

    tmp = v[i];
    v[i] = v[j];
    v[j] = tmp;
}

void reverse_lines(void *lines[], int num_lines)
{
    int i;
    for (i = 0; i < num_lines / 2; i++) {
        swap(lines, i, num_lines - 1 - i);
    }
}
/*
$ gcc -std=c90 -Wall main.c readlines.c alloc.c numcmp.c
*/

#include <stdio.h>
#include "alloc.h"

#define MAXLEN 1000

int getline(char* s, int lim);
void copy(char* to, char* from);

/* readlines: read input lines */
int readlines(char* lineptr[], int maxlines)
{
  int len, nlines;
  char* p, line[MAXLEN];

  nlines = 0;
  while ((len = getline(line, MAXLEN)) > 0 )
    if (nlines >= maxlines || (p = alloc(len)) == NULL)
      return -1;
    else {
        line[len-1] = '\0';
        copy(p, line);
        lineptr[nlines++] = p;
    }
  return nlines;
}

/* writelines: write output lines */
void writelines(char* lineptr[], int nlines)
{
  int i;

  for (i = 0; i < nlines; i++)
      printf("%s\n", lineptr[i]);
}


int getline(char* s, int lim)
{
  int c, i;

  for (i=0; i<lim-1 && (c=getchar()) != EOF && c != '\n'; ++i)
    *s++ = c;
  if (c == '\n') {
    *s++ = c;
    ++i;
  }
  *s = '\0'; /* array of characters terminated by '\0' */
  return i;
}


void copy(char* to, char* from)
{
  while ((*to++ = *from++) != '\0')
    ;
}

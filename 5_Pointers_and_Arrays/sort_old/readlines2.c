#include <stdio.h>
#include "alloc.h"

#define MAXLEN 1000

int getline(char* s, int lim);
void copy(char* to, char* from);

/* readlines: read input lines, store in linestor */
int readlines2(char* lineptr[], int maxlines, char* linestor, int maxstor)
{
  int len, nlines;
  char line[MAXLEN];      /* Buffer for the current line being read */
  char* p_stor = linestor; /* Pointer to the next free position in linestor */
  char* linestor_end = linestor + maxstor; /* Pointer to the end of linestor */

  nlines = 0;
  while ((len = getline(line, MAXLEN)) > 0) {
    /* Check if lineptr is full or if linestor doesn't have enough space for the current line
    // 'len' from getline includes the newline, but not the null terminator.
    // The string stored will be 'len' characters long after replacing '\n' with '\0'*/
    if (nlines >= maxlines || p_stor + len > linestor_end) {
      return -1;
    } else {
      line[len - 1] = '\0';
      copy(p_stor, line);   /* Copy the line into linestor */
      lineptr[nlines++] = p_stor; /* Store pointer to the line in linestor */
      p_stor += len;        /* Advance pointer in linestor by the length of the stored string (incl. '\0') */
    }
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

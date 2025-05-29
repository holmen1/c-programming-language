#include <stdio.h>


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

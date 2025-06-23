#include <ctype.h>
#include <stdio.h>

int getch(void);
void ungetch(int c);

/* getword: get next word or character from input */
int getword(char *word, int lim)
{

  int c, getch(void);
  void ungetch(int);

  char *w = word;

  while (isspace(c = getch()))
    ;

  if (c != EOF)
    *w++ = c;

  if (!isalpha(c)) {
    *w = '\0';
    return c;
  }

  for (; --lim > 0; w++)
    if (!isalnum(*w = getch())) {
      ungetch(*w);
      break;
    }

  *w = '\0';

  return word[0];
}

#define BUFSIZE 1 /* maximum buffer size */

char buf[BUFSIZE]; /* buffer for ungetch */

int bufp = 0;

int getch(void) /* get a (possibly pushed back) character */
{

  return bufp > 0 ? buf[--bufp] : getchar();
}

void ungetch(int c) /* push character back to input */
{
  if (c == EOF) {
    return; /* Do not push back EOF */
  }

  if (bufp >= BUFSIZE)
    printf("ungetch: too many characters\n");
  else
    buf[bufp++] = c;
}

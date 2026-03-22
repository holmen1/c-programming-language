#include <stdio.h>

#define BUFSIZE 1 /* maximum buffer size */

int buf[BUFSIZE]; /* buffer for ungetch */
int bufp = 0;

int getch(void) /* get a (possibly pushed back) character */
{
    return bufp > 0 ? buf[--bufp] : getchar();
}

void ungetch(int c) /* push character back to input */
{
    if (bufp >= BUFSIZE)
        fprintf(stderr, "ungetch: too many characters\n");
    else
        buf[bufp++] = c;
}

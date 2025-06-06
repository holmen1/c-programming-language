#include <stdio.h>

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
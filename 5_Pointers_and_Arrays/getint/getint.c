#include <ctype.h>
#include <stdio.h>
#include "getch.h"

int isnan(int c)
{
    if (!isdigit(c) && c != EOF && c != '+' && c != '-')
        return 1;
    return 0;
}

/* getint: get next integer from input into *pn */
int getint(int* pn)
{
    int c, sign;

    while (isspace(c = getch()))
        ;
    if (isnan(c)) {
        ungetch(c); /* not a number */
        return 0;
    }
    sign = (c == '-') ? -1 : 1;
    if (c == '+' || c == '-')
        c = getch();
    if (isnan(c)) {
        ungetch(c);
        return 0;
    }
    for (*pn = 0; isdigit(c); c = getch())
        *pn = 10 * *pn + (c - '0');
    *pn *= sign;
    if (c != EOF)
        ungetch(c);
    return c;
}


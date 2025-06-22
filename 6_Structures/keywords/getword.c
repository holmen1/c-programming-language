#include <ctype.h>
#include <stdio.h>

int getch(void);
void ungetch(int c);

/* getword: get next word or character from input */
int getword(char *word, int lim)
{
    int c, next_char;
    char *w = word;

    while (isspace(c = getch()))
        ;
    if (c == EOF)
        return EOF;

    *w++ = c;

    /* Handle string constants */
    if (c == '"') {
        for (; --lim > 0; w++) {
            *w = getch();
            if (*w == '\\') { /* Handle escaped characters */
                *++w = getch();
            } else if (*w == '"' || *w == EOF) {
                break;
            }
        }
        *w = '\0';
        return word[0];
    }

    /* Handle multi-line comments */
    if (c == '/') {
        if ((next_char = getch()) == '*') { /* Multi-line comment start */
            while ((c = getch()) != EOF) {
                if (c == '*') {
                    if ((next_char = getch()) == '/') {
                        break; /* End of comment */
                    } else {
                        ungetch(next_char);
                    }
                }
            }
            return getword(word, lim); /* Recursively get the next token */
        } else {
            ungetch(next_char); /* Not a comment, just a slash */
        }
    }

    /* Handle preprocessor directives */
    if (c == '#') {
        while ((c = getch()) != '\n' && c != EOF)
            ; /* Skip to end of line */
        return getword(word, lim); /* Get next token after the directive */
    }

    /* Handle identifiers (words) */
    if (isalpha(c) || c == '_') {
        for (; --lim > 0; w++) {
            if (!isalnum(*w = getch()) && *w != '_') {
                ungetch(*w);
                break;
            }
        }
        *w = '\0';
        return word[0];
    }

    /* Handle single-character tokens (operators, etc.) */
    *w = '\0';
    return c;
}

#define BUFSIZE 100 /* maximum buffer size */

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


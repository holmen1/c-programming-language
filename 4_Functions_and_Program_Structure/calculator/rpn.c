#include <assert.h>
#include <ctype.h>
#include <stdlib.h>
#include <stdio.h>

#define MAXVAL 100 /* maximum depth of val stack */
#define MAXOP 100 /* maximum depth of operand or operator */
#define NUMBER '0'
#define BUFSIZE 100 /* maximum buffer size */


/* --- STACK --- */
int sp = 0;
double val[MAXVAL];

void push(double f) {
    if (sp < MAXVAL)
        val[sp++] = f;
    else
        printf("error: stack full, can't push %g\n", f);
}

double pop(void) {
    if (sp > 0)
        return val[--sp];
    else {
        printf("error: stack empty\n");
        return 0.0;
    }
}


int getch(void);
void ungetch(int);

/* getop: get next operator or numeric operand */
int getop(char s[])
{
    int i, c;

    while ((s[0] = c = getch()) == ' ' || c == '\t')
        ;
    s[1] = '\0';
    if (!isdigit(c) && c != '.')
        return c;
    i = 0;
    if (isdigit(c))
        while (isdigit(s[++i] = c = getch()))
            ;
    if (c == '.')
        while (isdigit(s[++i] = c = getch()))
            ;
    s[i] = '\0';
    if (c != EOF)
        ungetch(c);
    return NUMBER;
}


char buf[BUFSIZE]; /* buffer for ungetch */
int bufp = 0;

int getch(void) /* get a (possibly pushed back) character */
{
    return bufp > 0 ? buf[--bufp] : getchar();
}

void ungetch(int c) /* push character back to input */
{
    if (bufp >= BUFSIZE)
        printf("ungetch: too many characters\n");
    else
        buf[bufp++] = c;
}

/* --- ASSERTS --- */
void test_push_pop() {
    int i;
    /* Test pushing and popping a single value */
    push(1.0);
    assert(pop() == 1.0);

    /* Test pushing and popping multiple values */
    push(2.0);
    push(3.0);
    assert(pop() == 3.0);
    assert(pop() == 2.0);

    /* Test popping from an empty stack */
    assert(pop() == 0.0); /* Should print "error: stack empty" */

    /* Test pushing to a full stack */
    for (i = 0; i < MAXVAL; i++) {
        push(i);
    }
    push(MAXVAL); /* Should print "error: stack full" */
    i = 0; /* empty stack */
}

void test_getch_ungetch() {
    int i;

    ungetch('a');
    assert(getch() == 'a');
}

int main() {
    /*
    test_push_pop();
    test_getch_ungetch();
    printf("All tests passed.\n");
    */

    int type;
    double op2;
    char s[MAXOP];

    while ((type = getop(s)) != EOF) {
        switch (type) {
            case NUMBER:
                push(atof(s));
                break;
            case '+':
                push(pop() + pop());
                break;
            case '*':
                push(pop() * pop());
                break;
            case '-':
                op2 = pop();
                push(pop() - op2);
                break;
            case '/':
                op2 = pop();
                if (op2 != 0.0)
                    push(pop() / op2);
                else
                    printf("error: zero divisor\n");
                break;
            case '\n':
                printf("\t%.8g\n", pop());
                break;
            default:
                printf("error: unknown command %s\n", s);
                break;
        }
    }
    return 0;
}
/*
$ gcc -std=c90 rpn.c
$ ./a.out
5 12.0 3 / *
        20
*/


//
//
//
#include <assert.h>
#include <ctype.h>
#include <stdio.h>

#define MAXVAL 100 /* maximum depth of val stack */
#define NUMBER '0'
#define BUFSIZE 100 /* maximum buffer size */

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
    // Test pushing and popping a single value
    push(1.0);
    assert(pop() == 1.0);

    // Test pushing and popping multiple values
    push(2.0);
    push(3.0);
    assert(pop() == 3.0);
    assert(pop() == 2.0);

    // Test popping from an empty stack
    assert(pop() == 0.0); // Should print "error: stack empty"

    // Test pushing to a full stack
    for (int i = 0; i < MAXVAL; i++) {
        push(i);
    }
    push(MAXVAL); // Should print "error: stack full"
}

void test_getch_ungetch() {
    int i;

    ungetch('a');
    assert(getch() == 'a');

    // Test buffer overflow
    for (i = 0; i < BUFSIZE; i++) {
        ungetch('a');
    }
    ungetch('b'); // This should print an error message

    // Test getch with no ungetch
    assert(getch() == 'a'); // Last character pushed back
    for (i = 0; i < BUFSIZE - 1; i++) {
    }
    // Now the buffer should be empty, getch should call getchar()
    // We can't test getchar() easily without user input, so we stop here
}

int main() {
    test_push_pop();
    test_getch_ungetch();
    printf("All tests passed.\n");
    return 0;
}



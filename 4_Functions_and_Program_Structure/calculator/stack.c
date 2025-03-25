#include <stdio.h>

#define MAXVAL 100 /* maximum depth of val stack */

int sp = 0; /* next available position in the stack */
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
    printf("error: stack empty\n");
    return 0.0;
}

void peek(void) {
    if (sp < 0)
        printf("error: stack underflow\n");
    else
        printf("Top of stack: %g\n", val[sp - 1]);
}
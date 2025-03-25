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
    if (sp > MAXVAL)
        printf("error: stack overflow\n");
    else if (sp > 0)
        printf("top of stack: %.8g\n", val[sp - 1]);
    else
        printf("stack underlow\n");
}

void swap(void) {
    if (sp > 1) {
        double temp = val[sp - 1];
        val[sp - 1] = val[sp - 2];
        val[sp - 2] = temp;
    } else {
        printf("error: not enough elements to swap\n");
    }
}

void clear(void) {
    sp = 0;
}
#include <math.h>
#include <stdlib.h>
#include <stdio.h>
#include "calc.h"

#define MAXOP 100 /* maximum depth of operand or operator */

int main() {

    int type;
    double op1, op2;
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
            case '%':
                op2 = pop();
                op1 = pop();
            if (op2 != 0.0)
                push(op1 - op2 * (int)(op1 / op2));
            else
                printf("error: zero divisor\n");
            break;
            case 's':
                push(sin(pop()));
            break;
            case 'e':
                push(exp(pop()));
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
$ gcc -std=c90 -Wall -o calculator getch.c getop.c stack.c main.c -lm
*/

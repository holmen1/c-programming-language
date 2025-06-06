#include <stdlib.h>
#include <stdio.h>
#include "stack.h"

int is_numeric_string(const char *s);

int main(int argc, char *argv[]) {
    char *token; /* buffer for operand or operator */
    double op2;

    if (argc == 1) {
        printf("Usage: %s <rpn_expression>\n", argv[0]);
        return 1;
    }

    while (--argc > 0) {
        token = *++argv;

        if (is_numeric_string(token))
            push(atof(token));
        else if (*token != '\0' && *(token + 1) == '\0')
            switch (*token) {
                case '+':
                    push(pop() + pop());
                break;
                case 'x':
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
                default:
                    printf("error: unknown command %s\n", token);
                break;
            }
    }
    printf("\t%.8g\n", pop());
    return 0;
}

int is_numeric_string(const char *s) {
    char *endptr;

    strtod(s, &endptr);

    /* A successful conversion means:
    // 1. endptr was advanced (conversion happened).
    // 2. The character pointed to by endptr is the null terminator (entire string was consumed). */
    if (endptr != s && *endptr == '\0') {
        return 1;
    }

    return 0;
}

/*
$ gcc -std=c90 -Wall -o rpn main.c stack.c
*/

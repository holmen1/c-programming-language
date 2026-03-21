#include "calc.h"
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
    case MATHOP:
      if (strcmp(s, "exp") == 0)
        push(exp(pop()));
      else if (strcmp(s, "pow") == 0) {
        op2 = pop();
        push(pow(pop(), op2));
      } else if (strcmp(s, "sin") == 0)
        push(sin(pop()));
      else if (strcmp(s, "cos") == 0)
        push(cos(pop()));
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
        fprintf(stderr, "error: zero divisor\n");
      break;
    case '%':
      op2 = pop();
      op1 = pop();
      if (op2 != 0.0)
        push(op1 - op2 * (int)(op1 / op2));
      else
        fprintf(stderr, "error: zero divisor\n");
      break;
    case 'P':
      printf("\t%.8g\n", peek());
      break;
    case 'D':
      push(peek());
      break;
    case 'S':
      swap();
      break;
    case 'C':
      clear();
      break;
    case '\n':
      printf("\t%.8g\n", pop());
      break;
    default:
      fprintf(stderr, "error: unknown command %s\n", s);
      break;
    }
  }
  return 0;
}

/*
$ gcc -std=c90 -Wall -o calculator getch.c getop.c stack.c main.c -lm
*/

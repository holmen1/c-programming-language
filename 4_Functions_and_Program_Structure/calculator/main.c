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
  double var[26] = {0.0};
  int vindex = -1;

  while ((type = getop(s)) != EOF) {
    switch (type) {
    case NUMBER:
      push(atof(s));
      break;
    case VARIABLE:
      vindex = s[0] - 'a';
      push(var[vindex]);
      break;
    case '=':
      if (vindex >= 0) {
        pop(); /* discard placeholder pushed by VARIABLE */
        var[vindex] = pop();
      }
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
      var['z' - 'a'] = peek();
      printf("\t%.8g\n", var['z' - 'a']);
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
      var['z' - 'a'] = pop();
      printf("\t%.8g\n", var['z' - 'a']);
      break;
    default:
      fprintf(stderr, "error: unknown command %s\n", s);
      break;
    }
  }
  return 0;
}

/*
$ gcc -std=c90 -Wall -g -o calculator stack.c getch.c getop.c main.c -lm
*/

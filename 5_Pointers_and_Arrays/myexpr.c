#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#define MAXVAL 100 /* maximum depth of val stack */

int sp = 0; /* next available position in the stack */
int stack[MAXVAL];

int isint(const char *s);

int main(int argc, char *argv[]) {
  char *token;
  int op2;
  while (--argc) {
    token = *++argv;
    if (isint(token))
      stack[sp++] = atoi(token);
    else {
      switch (*token) {
      case '+':
        op2 = stack[--sp];
        stack[sp - 1] += op2;
        break;
      case '-':
        op2 = stack[--sp];
        stack[sp - 1] -= op2;
        break;
      case 'x':
        op2 = stack[--sp];
        stack[sp - 1] *= op2;
        break;
      default:
        fprintf(stderr, "Unknown operator %s\n", token);
        return 1;
      }
    }
  }
  printf("%d\n", stack[0]);
  return 0;
}

int isint(const char *s) {
  char c;
  while ((c = *s++) != '\0') {
    if (!isdigit(c))
      return 0;
  }
  return 1;
}

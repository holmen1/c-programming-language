#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>

#define MAXVAL 100 /* maximum depth of val stack */

int sp = 0; /* next available position in the stack */
int stack[MAXVAL];

void _push(int i);
int _pop(void);
int getnum(const char *s);

char *token;
int main(int argc, char *argv[]) {
  int len;
  while (--argc) {
    token = *++argv;
    if ((len = getnum(token))) {
      printf("token = %s len = %d\n", token, len);
      _push(atoi(token));
    }
  }

  return 0;
}

void _push(int i) {
  if (sp < MAXVAL)
    stack[sp++] = i;
  else
    fprintf(stderr, "error: stack full, can't push %d\n", i);
}

int _pop(void) {
  if (sp > 0)
    return stack[--sp];
  fprintf(stderr, "error: stack empty\n");
  return -99;
}

int getnum(const char *s) {
  char c;
  int len = 0;
  while ((c = *s++) != '\0') {
    if (!isdigit(c))
      return 0;
    len++;
  }

  return len;
}

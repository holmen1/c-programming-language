#include "calc.h"
#include <ctype.h>
#include <stdio.h>

int getop(char s[]) {
  int i = 0;
  int c;
  static int lastc = 0;

  c = (lastc == 0) ? getchar() : lastc;
  lastc = 0;

  while ((s[0] = c) == ' ' || c == '\t')
    c = getchar();

  s[1] = '\0';
  if (c == '\n')
    return c;
  if (islower(c)) {
    while (islower(s[++i] = c = getchar()))
      ;
    lastc = c;
    s[i] = '\0';
    return (i == 1) ? VARIABLE : MATHOP;
  }
  if (c == '-') {
    int next = getchar();
    if (isdigit(next) || next == '.')
      s[++i] = c = next;
    else {
      lastc = next;
      return '-';
    }
  }
  if (!isdigit(c) && c != '.')
    return c;
  if (isdigit(c))
    while (isdigit(s[++i] = c = getchar()))
      ;
  if (c == '.')
    while (isdigit(s[++i] = c = getchar()))
      ;
  s[i] = '\0';
  lastc = c;
  return NUMBER;
}

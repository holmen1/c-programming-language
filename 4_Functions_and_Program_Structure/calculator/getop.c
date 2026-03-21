#include "calc.h"
#include <ctype.h>

int getop(char s[]) {
  int i = 0;
  static int c;
  while ((s[0] = c = getch()) == ' ' || c == '\t')
    ;
  s[1] = '\0';
  if (c == '\n')
    return c;
  if (islower(c)) {
    while (islower(s[++i] = c = getch()))
      ;
    ungetch(c);
    s[i] = '\0';
    return MATHOP;
  }
  if (c == '-') {
    int next = getch();
    if (isdigit(next) || next == '.')
      s[++i] = c = next;
    else {
      ungetch(next);
      return '-';
    }
  }
  if (!isdigit(c) && c != '.')
    return c;
  if (isdigit(c))
    while (isdigit(s[++i] = c = getch()))
      ;
  if (c == '.')
    while (isdigit(s[++i] = c = getch()))
      ;
  s[i] = '\0';
  return NUMBER;
}

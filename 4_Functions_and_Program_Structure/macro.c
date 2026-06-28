#include <stdio.h>

#define swap(t, x, y)                                                          \
  {                                                                            \
    t temp;                                                                    \
    temp = x;                                                                  \
    x = y;                                                                     \
    y = temp;                                                                  \
  }

int main(void) {
  char *s = "hello";
  char *t = "world";
  printf("s=%s\tt=%s\n", s, t);
  printf("swap(char*, s, t)\n");
  swap(char *, s, t);
  printf("s=%s\tt=%s\n", s, t);

  return 0;
}


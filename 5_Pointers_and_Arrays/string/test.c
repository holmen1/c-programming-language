#include <stdio.h>

void _strcat(char *s, const char *t);

int main(void) {
  char dst[10] = "hello";
  const char src[] = "world";

  printf("dst = %s\n", dst);
  printf("src = %s\n", src);
  printf("strcat(dst, src)\n");
  _strcat(dst, src);
  printf("dst = %s\n", dst);

  return 0;
}

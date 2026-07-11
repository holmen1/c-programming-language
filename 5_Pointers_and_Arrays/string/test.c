#include <stdio.h>

void _strcat(char *s, const char *t);
int strend(char *s, char *t);

int main(void) {
  char dst[10] = "hello";
  char src[] = "world";

  printf("dst = %s\n", dst);
  printf("src = %s\n", src);
  printf("strcat(dst, src)\n");
  _strcat(dst, src);
  printf("dst = %s\n", dst);

  printf("strend(dst, src) = %d\n", strend(dst, src));
  printf("strend(src, dst) = %d\n", strend(src, dst));
  printf("strend(src, src) = %d\n", strend(src, src));

  return 0;
}

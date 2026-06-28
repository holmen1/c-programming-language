#include <stdio.h>

#define MAXLEN 6

static int i = 0;

void itoa_helper(int n, char s[]) {
  if (n < 0) {
    s[i++] = '-';
    n = -n;
  }
  if (n / 10)
    itoa_helper(n / 10, s);
  if (i < MAXLEN - 1)
    s[i++] = n % 10 + '0';
  else
    fprintf(stderr, "%d to long, result is truncated\n", n);
}

void itoa(int n, char s[]) {
  itoa_helper(n, s);
  s[i] = '\0';
  i = 0;
}

int main(int argc, char *argv[]) {
  char res[MAXLEN];
  int examples[] = {123, -1234, 12345, 123456, -12345};
  int k = 0;
  for (; k < 5; k++) {
    printf("itoa(%d, s)\n", examples[k]);
    itoa(examples[k], res);
    printf("s = %s\n", res);
  }

  return 0;
}

#include <stdio.h>

#define MAXLEN 6

static int itoa_rec(int n, char s[], size_t size, int *i) {
  if (n / 10)
    itoa_rec(n / 10, s, size, i);
  if (*i + 1 < size) {
    s[(*i)++] = '0' + n % 10;
    return 1;
  } else
    return 0;
}

int itoa(int n, char s[], size_t size) {
  int i = 0;
  int is_success;
  if (n < 0) {
    s[i++] = '-';
    n = -n;
  }
  is_success = itoa_rec(n, s, size, &i);
  s[i] = '\0';
  return is_success;
}

int main(int argc, char *argv[]) {
  char res[MAXLEN];
  int examples[] = {123, -1234, 12345, 123456, -12345};
  int k = 0;
  for (; k < 5; k++) {
    if (!itoa(examples[k], res, sizeof res))
      fprintf(stderr, "%d to long, result is truncated\n", examples[k]);
    printf("itoa(%d) -> %s\n", examples[k], res);
  }
  return 0;
}

#include <stdio.h>

#define MAXLEN 200

static void itoa_rec(int n, char s[], int *i) {
  if (n / 10)
    itoa_rec(n / 10, s, i);
  s[(*i)++] = (char)('0' + n % 10);
}

void itoa(int n, char s[]) {
  int i = 0;
  if (n < 0) {
    s[i++] = '-';
    n = -n;
  }
  itoa_rec(n, s, &i);
  s[i] = '\0';
}

int main(void) {
  char buf[MAXLEN];
  int examples[] = {123, -1234, 0};
  int k = 0;
  for (; k < 3; k++) {
    itoa(examples[k], buf);
    printf("itoa(%d) -> %s\n", examples[k], buf);
  }

  return 0;
}

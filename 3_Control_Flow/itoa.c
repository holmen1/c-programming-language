#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void itoa(int n, char s[]);
static void itoa2(int n, char s[]);

int main(void) {
  char s[64];
  int tests[] = {32, -64, 128, INT_MAX, INT_MIN };

  printf("Word size: %d bits\n", (int)sizeof(long) * CHAR_BIT);
  printf("int size: %zu bits\n", sizeof(int) * CHAR_BIT);
  printf("INT_MAX: %d\n", INT_MAX);
  printf("INT_MIN: %d\n", INT_MIN);

  printf("\nTesting itoa:\n");
  for (int i = 0; i < 5; i++) {
    itoa(tests[i], s);
    printf("%d -> %s\n", tests[i], s);
  }

  printf("\nTesting itoa2:\n");
  for (int i = 0; i < 5; i++) {
    itoa2(tests[i], s);
    printf("%d -> %s\n", tests[i], s);
  }

  return 0;
}

/* reverse: reverse string s in place */
static void reverse(char s[]) {
  int c, i, j;

  for (i = 0, j = strlen(s) - 1; i < j; i++, j--) {
    c = s[i];
    s[i] = s[j];
    s[j] = c;
  }
}

/* convert n to characters in s */
void itoa(int n, char s[]) {
  int i, sign;

  if ((sign = n) < 0)
    n = -n;

  i = 0;
  do
    s[i++] = n % 10 + '0';
  while ((n /= 10) > 0);

  if (sign < 0)
    s[i++] = '-';
  s[i] = '\0';

  return reverse(s);
}

void itoa2(int n, char s[]) {
  int i, sign;

  sign = n;

  i = 0;
  do
    s[i++] = abs(n % 10) + '0';
  while ((n /= 10) != 0);

  if (sign < 0)
    s[i++] = '-';
  s[i] = '\0';

  return reverse(s);
}

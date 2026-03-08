#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void itoa(int n, char s[]);
static void itoa2(int n, char s[]);
static void itob(int n, char s[], int b);

int main(void) {
  char s[64]; /* needs at least 43 for 32-bit binary: "0b " + 32 digits + 7 spaces + '\0' */
  int tests[] = {32, -64, 128, INT_MAX, INT_MIN};

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

  printf("\nTo hex:\n");
  for (int i = 0; i < 5; i++) {
    itob(tests[i], s, 16);
    printf("%d -> %s\n", tests[i], s);
  }

  printf("\nTo oct:\n");
  for (int i = 0; i < 5; i++) {
    itob(tests[i], s, 8);
    printf("%d -> %s\n", tests[i], s);
  }

  printf("\nTo bin:\n");
  for (int i = 0; i < 5; i++) {
    itob(tests[i], s, 2);
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

/* itob: convert n to base-b string in s.
   Treats n as unsigned (raw bit pattern — no sign).
   Hex: uppercase, zero-padded to 8 digits  e.g. 0xFFFFFFCC
   Oct: leading zero prefix                 e.g. 037777777700
   Bin: zero-padded, nibbles separated      e.g. 0b 11111111 ... 11000000 */
void itob(int n, char s[], int b) {
  unsigned int u = (unsigned int)n;
  char digits[32]; /* raw digits, least significant first */
  int nd = 0;      /* digit count */
  int width;       /* zero-pad target width */
  int i, j;

  /* natural widths for zero-padding */
  if (b == 16)
    width = sizeof(int) * 2;         /* 8 hex digits for 32-bit int */
  else if (b == 2)
    width = sizeof(int) * CHAR_BIT;  /* 32 binary digits for 32-bit int */
  else
    width = 0;                       /* no padding for octal or other bases */

  /* collect digits least significant first */
  do {
    int d = u % b;
    digits[nd++] = (b == 16 && d >= 10) ? 'A' + d - 10 : '0' + d;
  } while ((u /= b) != 0);

  /* zero-pad up to target width */
  while (nd < width)
    digits[nd++] = '0';

  /* write prefix */
  i = 0;
  if (b == 16) {
    s[i++] = '0'; s[i++] = 'x';
  } else if (b == 8) {
    s[i++] = '0';
  } else if (b == 2) {
    s[i++] = '0'; s[i++] = 'b'; s[i++] = ' ';
  }

  /* write digits most significant first;
     for binary insert a space before each byte boundary */
  for (j = nd - 1; j >= 0; j--) {
    if (b == 2 && j < nd - 1 && (nd - 1 - j) % 8 == 0)
      s[i++] = ' ';
    s[i++] = digits[j];
  }
  s[i] = '\0';
}

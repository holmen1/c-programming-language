/* Exercise 3-5 */
#include <stdio.h>
#include <string.h>

char itoh(int n);
void reverse(char s[]);


/* itob: convert integer n into base b character */
void itob(int n, char s[], int b)
{
    int c, i;

    i = 0;
    do {
      if (b == 16)
        c = itoh(n % b);
      else
        c = n % b + '0';
      s[i++] = c;
    } while ((n /= b) > 0);
    s[i] = '\0';
    reverse(s);
}

int main()
{
  int x = 231;
  char str[12];

  printf("x = %d\n", x);
  itob(x, str, 10);
  printf("itob(x, str, 10):\t%s\n", str);
  itob(x, str, 2);
  printf("itob(x, str, 2):\t%s\n", str);
  itob(x, str, 16);
  printf("itob(x, str, 16):\t%s\n", str);


  return 0;
}

char itoh(int n)
{
  if (n < 10)
    return n + '0';
  switch (n) {
  case 10: case 11: case 12: case 13: case 14: case 15:
    return 'a' + n - 10;
  default: return '?';
  }
}

/* reverse: reverse string s in place */
void reverse(char s[])
{
    int c, i, j;

    for (i = 0, j = strlen(s) - 1; i < j; i++, j--) {
        c = s[i];
        s[i] = s[j];
        s[j] = c;
    }
}
/*
x = 231
itob(x, str, 10):	231
itob(x, str, 2):	11100111
itob(x, str, 16):	e7
*/
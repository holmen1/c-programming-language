#include <stdio.h>
#include <string.h>

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

int main()
{
  char str[] = "Hello World!";
  printf("str: %s\n", str);
  reverse(str);
  printf("reverse(str): %s\n", str);

  return 0;
}
/*
str: Hello World!
reverse(str): !dlroW olleH
 */
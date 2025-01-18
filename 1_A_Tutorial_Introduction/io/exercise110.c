#include <stdio.h>
/*
replace tab with \t, backspace by \b
*/
int main()
{
  int c, specs;

  specs = 0;
  while ((c = getchar()) != EOF)
    if (c == '\t')
    {
      putchar('\\');
      putchar('t');
    }
    else if (c == '\b')
    {
      putchar('\\');
      putchar('b');
    }
    else if (c == '\\')
    {
      putchar('\\');
      putchar('\\');
    }
    else
     putchar(c);

  return 0;
}

/*
$ gcc -std=c89 exercise110.c

*/

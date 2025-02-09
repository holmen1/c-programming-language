/* Exercise 2-4 */
#include <stdio.h>


/* lower: convert c to lower case; ASCII only */
int lower(int c)
{
  return (c >= 'A' && c <= 'Z') ? c + 'a' - 'A' : c;
}

void lowerall(char s[])
{
  int i;
  for (i = 0; s[i] != '\0'; ++i)
    s[i] = lower(s[i]);
  s[++i] = '\0';
}


int main()
{
  char str[] = "Hello, World!";
  printf("str = %s\n", str);
  lowerall(str);
  printf("lowerall(str)\n");
  printf("str = %s\n", str);

  return 1;
}
/*
$ gcc -g -Wall -std=c90 lower.c 
$ ./a.out 
str = Hello, World!
lowerall(str)
str = hello, world!
*/ 

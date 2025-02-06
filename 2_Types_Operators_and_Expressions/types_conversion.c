/* Exercise 2-3 */
#include <stdio.h>


/* atoi: convert s to integer */
int atoi(char s[])
{
  int i, n;

  n = 0;
  for (i = 0; s[i] >= '0' && s[i] <= '9'; ++i)
    n = 10 * n + (s[i] - '0');

  return n;
}

/* htoi: convert hex string to integer */
int htoi(char s[])
{
  int i, n;

  if (s[0] == '0' && s[1] == 'x') {
    n = 0;
    for (i = 2; (s[i] >= '0' && s[i] <= '9') || (s[i] >= 'a' && s[i] <= 'f'); ++i)
    {
      if (s[i] <= '9')
        n = 16 * n + (s[i] - '0');
      else
        n = 16 * n + (s[i] - 'W');
    }
    return n;
  } else
      return -1;
}

int main()
{
  printf("atoi: convert s to integer\n");
  printf("atoi(\"312\")\t=\t%d\n", atoi("312"));
  
  printf("htoi: convert s to integer\n");
  printf("htoi(\"0x138\")\t=\t%d\n", htoi("0x138"));
  printf("htoi(\"0xe7\")\t=\t%d\n", htoi("0xe7"));
  printf("htoi(\"0x4c3f8\")\t=\t%d\n", htoi("0x4c3f8"));
  
  return 1;
}
/*
$ gcc -g -Wall -std=c90 types_conversion.c 
$ ./a.out 
atoi: convert s to integer
atoi("312")     =       312
htoi: convert s to integer
htoi("0x138")   =       312
htoi("0xe7")    =       231
htoi("0x4c3f8") =       312312
*/ 

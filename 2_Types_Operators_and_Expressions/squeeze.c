/* Exercise 2-4 */
#include <stdio.h>


/* squeeze: delete all c from s */
void squeeze(char s[], int c)
{
  int i, j;

  for (i = j = 0; s[i] != '\0'; ++i)
    if (s[i] != c)
      s[j++] = s[i];
  s[j] = '\0';
}

/* squeeze2: delete all characters in d from s */
void squeeze2(char s[], char d[])
{
  int i;

  for (i = 0; d[i] != '\0'; ++i)
    squeeze(s, d[i]);
}


int main()
{
  char str[] = "Hello, World!";
  printf("str = %s\n", str);
  squeeze(str, 'l');
  printf("squeeze(str, 'l')\n");
  printf("str = %s\n", str);
  
  char str2[] = "Hello, World!";
  printf("str2 = %s\n", str2);
  squeeze2(str2, "Wo");
  printf("squeeze2(str2, \"Wo\")\n");
  printf("str2 = %s\n", str2);

  return 1;
}
/*
$ gcc -g -Wall -std=c90 squeeze.c 
$ ./a.out 
str = Hello, World!
squeeze(str, 'l')
str = Heo, Word!
str2 = Hello, World!
squeeze2(str2, "Wo")
str2 = Hell, rld!
*/ 

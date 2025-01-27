#include <stdio.h>
#define MAXLINE 1000

int get_line(char s[], int maxline);
void reverse(char to[], char from[], int l);

int main()
{
  char line[MAXLINE];
  char trimmed[MAXLINE];
  int length;

  while ((length = get_line(line, MAXLINE)) > 0)
  {
    if (length > 2) {
      reverse(trimmed, line, length);
      printf("%s", trimmed);
    }
    else
      printf("%s", line);
  }

  return 0;
}

int get_line(char s[], int maxline)
{
  int c, i;

  for (i=0; i<maxline-1 && (c=getchar()) != EOF && c != '\n'; ++i)
    s[i] = c;
  if (c == '\n') {
    s[i] = c;
    ++i;
  }
  s[i] = '\0'; /* array of characters terminated by '\0' */
  return i;
}

void reverse(char to[], char from[], int l)
{
  int c,i,j;

  i = 0;
  j = l;

  to[j] = '\0';
  --j;
  to[j] = '\n';
  --j;
  while ((c = from[i]) != '\0')
  {
    to[j] = c;
    ++i;
    --j;
  }
}

/*
$ gcc -std=c90 reverse.c 
$ ./a.out < hello.txt 
!dlroW olleH
!dlroW olleH  

staM olleH
a
ba
*/


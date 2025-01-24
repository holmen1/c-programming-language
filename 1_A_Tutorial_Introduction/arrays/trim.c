#include <stdio.h>
#define MAXLINE 1000
#define TRAILING 0
#define LINE 1

int get_line(char line[], int maxline);
void trim(char to[], char from[]);

int main()
{
  char line[MAXLINE];
  char trimmed[MAXLINE];

  while (get_line(line, MAXLINE) > 0)
  {
    trim(trimmed, line);
    printf("%s", trimmed);
  }

  return 0;
}


int get_line(char s[], int lim)
{
  int c, i;

  for (i=0; i<lim-1 && (c=getchar()) != EOF && c != '\n'; ++i)
    s[i] = c;
  if (c == '\n') {
    s[i] = c;
    ++i;
  }
  s[i] = '\0'; /* array of characters terminated by '\0' */
  return i;
}

void trim(char to[], char from[])
{
  int c,i,j;
  int state = TRAILING;
  i = 0;
  while ((c = from[i]) != '\0')
  {
    if (state == TRAILING && (c != ' ' || c != '\t')) {
      state = LINE;
      j = 0;
    }

    if (state == LINE) {
      to[j] = c;
      ++j;
    }
    ++i;
  }
  to[j] = '\0';
}

/*
$ gcc -std=c89 longest.c -o longest
$ ./longest < longest.c 
  for (i=0; i<lim-1 && (c=getchar()) != EOF && c != '\n'; ++i)
*/


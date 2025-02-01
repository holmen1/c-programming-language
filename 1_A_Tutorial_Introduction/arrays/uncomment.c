#include <stdio.h>
#define MAXLINE 1000
#define COMMENT 0
#define LINE 1

int get_line(char s[], int maxline);
void uncomment(char to[], char from[]);
int state;

int main()
{
  char line[MAXLINE];
  char trimmed[MAXLINE];
  extern int state;
  state = LINE;

  while (get_line(line, MAXLINE) > 0)
  {
    uncomment(trimmed, line);
    printf("%s", trimmed);
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

void uncomment(char to[], char from[])
{
  int c, i, j;
  extern int state;
  i = j = 0;
  while ((c = from[i]) != '\n')
  {
    if (c == '/' && from[i+1] == '*')
      state = COMMENT;
    else if (c == '*' && from[i+1]  == '/') {
      state = LINE;
      i = i + 2;
    }

    if (state == LINE && (c = from[i]) != '\n') {
      to[j] = c;
      ++j;
    }
    ++i;
  }
  to[j] = '\n';
  to[j+1] = '\0';
}

/*
$ gcc -g -Wall -std=c90 uncomment.c
$ ./a.out < hello.txt 
Hello World!
  Hello  World!

Hello Mats
a
ab
*/


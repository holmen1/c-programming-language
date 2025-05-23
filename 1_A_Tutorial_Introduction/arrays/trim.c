#include <stdio.h>
#define MAXLINE 1000
#define TRAILING 0
#define LINE 1

int get_line(char s[], int maxline);
void trim(char to[], char from[]);

int main()
{
  char line[MAXLINE];
  char trimmed[MAXLINE];

  while (get_line(line, MAXLINE) > 0)
  {
    if ('\n' != line[0]) {
      trim(trimmed, line);
      printf("%s", trimmed);
    }
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

void trim(char to[], char from[])
{
  int c,i,j;
  int state = TRAILING;
  i = j = 0;
  while ((c = from[i]) != '\0')
  {
    if (state == TRAILING && c != ' ')
      state = LINE;

    if (state == LINE) {
      to[j] = c;
      ++j;
    }
    ++i;
  }
  to[j] = '\0';
}

/*
$ gcc -std=c90 trim.c 
$ ./a.out < trim.c 
#include <stdio.h>
#define MAXLINE 1000
#define TRAILING 0
#define LINE 1
int get_line(char s[], int maxline);
void trim(char to[], char from[]);
int main()
{
char line[MAXLINE];
char trimmed[MAXLINE];
while (get_line(line, MAXLINE) > 0)
{
if ('\n' != line[0]) {
trim(trimmed, line);
printf("%s", trimmed);
}
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
s[i] = '\0'; 
return i;
}
void trim(char to[], char from[])
{
int c,i,j;
int state = TRAILING;
i = j = 0;
while ((c = from[i]) != '\0')
{
if (state == TRAILING && c != ' ')
state = LINE;
if (state == LINE) {
to[j] = c;
++j;
}
++i;
}
to[j] = '\0';
}
*/


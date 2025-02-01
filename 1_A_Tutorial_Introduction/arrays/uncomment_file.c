#include <stdio.h>
#define MAXLINE 1000
#define COMMENT 0
#define LINE 1

int get_file(char s[], int maxline);
void uncomment(char to[], char from[]);
int state;

int main()
{
  char file[MAXLINE];
  char uncommented[MAXLINE];
  extern int state;
  state = LINE;

  get_file(file, MAXLINE);
  uncomment(uncommented, file);
  printf("%s", uncommented);

  return 0;
}

int get_file(char s[], int maxline)
{
  int c, i;

  for (i=0; i<maxline-1 && (c=getchar()) != EOF; ++i)
    s[i] = c;
  
  s[i + 1] = '\0'; /* array of characters terminated by '\0' */
  return i;
}

void uncomment(char to[], char from[])
{
  int c, i, j;
  extern int state;
  i = j = 0;
  while ((c = from[i]) != EOF)
  {
    if (c == '/' && from[i+1] == '*')
      state = COMMENT;
    else if (c == '*' && from[i+1]  == '/') {
      state = LINE;
      i = i + 2;
    }

    if (state == LINE)
      to[j++] = from[i];

    ++i;
  }
  to[j] = '\0';
}

/*
$ gcc -g -Wall -std=c90 uncomment_file.c
$ ./a.out < hello.txt 

Hello World!
  Hello  World!

Hello Mats
a

ab



last line
*/

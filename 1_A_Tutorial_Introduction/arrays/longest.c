#include <stdio.h>
#define MAXLINE 1000

int getline(char line[], int maxline);
void copy(char to[], char from[]);

int main()
{
  int len;  /* current line length */
  int max;  /* max length so far */
  char line[MAXLINE];
  char longest[MAXLINE];

  max = 0;
  while ((len = getline(line, MAXLINE)) > 0)
    if (len > max) {
      max = len;
      copy(longest, line);
    }
  if (max > 0)
    printf("%s", longest);

  return 0;
}


int getline(char s[], int lim)
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


void copy(char to[], char from[])
{
  int i;

  i = 0;
  while ((to[i] = from[i]) != '\0')
    ++i;
}
/*
$ gcc -std=c89 longest.c -o longest
$ ./longest < longest.c 
  for (i=0; i<lim-1 && (c=getchar()) != EOF && c != '\n'; ++i)
*/

/* LESSONS LEARNED
Need c89 since gcc defaults to c17

$ gcc longest.c 
longest.c:4:5: error: conflicting types for ‘getline’; have ‘int(char *, int)’
    4 | int getline(char line[], int maxline);
      |     ^~~~~~~
In file included from longest.c:1:
/usr/include/stdio.h:697:18: note: previous declaration of ‘getline’ with type ‘__ssize_t(char ** restrict,  size_t * restrict,  FILE * restrict)’ {aka ‘long int(char ** restrict,  long unsigned int * restrict,  FILE * restrict)’}
  697 | extern __ssize_t getline (char **__restrict __lineptr,
      |                  ^~~~~~~
longest.c:27:5: error: conflicting types for ‘getline’; have ‘int(char *, int)’
   27 | int getline(char s[], int lim)
      |     ^~~~~~~
/usr/include/stdio.h:697:18: note: previous declaration of ‘getline’ with type ‘__ssize_t(char ** restrict,  size_t * restrict,  FILE * restrict)’ {aka ‘long int(char ** restrict,  long unsigned int * restrict,  FILE * restrict)’}
  697 | extern __ssize_t getline (char **__restrict __lineptr,
*/

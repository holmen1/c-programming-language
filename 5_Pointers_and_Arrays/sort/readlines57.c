#include <stdio.h>

#define MAXLEN 1000

int _getline(char *s, int lim);
void strcopy(char *to, char *from);

/* readlines: read input lines */
int readlines(char *lineptr[], int maxlines, char *linestorage, int maxstorage) {
  int len, nlines;
  char line[MAXLEN];
  char* storage_end = linestorage + maxstorage;

  nlines = 0;
  while ((len = _getline(line, MAXLEN)) > 0)
    if (nlines >= maxlines || linestorage + len > storage_end)
      return -1;
    else {
      line[len - 1] = '\0';
      strcopy(linestorage, line);
      lineptr[nlines++] = linestorage;
      linestorage += len;
    }
  return nlines;
}

/* writelines: write output lines */
void writelines(char *lineptr[], int nlines) {
  int i;

  for (i = 0; i < nlines; i++)
    printf("%s\n", lineptr[i]);
}

int _getline(char *s, int lim) {
  int c, i;

  i = 0;
  while (--lim > 0 && (c = getchar()) != EOF && c != '\n')
    s[i++] = c;
  if (c == '\n')
    s[i++] = c;
  s[i] = '\0'; /* array of characters terminated by '\0' */
  return i;
}

void strcopy(char *to, char *from) {
  while ((*to++ = *from++) != '\0')
    ;
}

#include <stdio.h>
#include <stdlib.h>

#define TABN 8
#define MAXLINELEN 100

void entabln(char *spacedln, const char *ln, int tablen);
char *entab(char *spacedln, int steps);

int main(int argc, char *argv[]) {
  char *buffer = NULL;
  size_t bufsize = 0; // Size of the buffer
  ssize_t c;          // Number of characters read
  char spaced[MAXLINELEN];

  while ((c = getline(&buffer, &bufsize, stdin)) != -1) {
    if (c > MAXLINELEN) {
      free(buffer);
      return 1;
    }

    entabln(spaced, buffer, TABN);
    printf("%s", spaced);
  }
  free(buffer);
  return 0;
}

void entabln(char *spacedln, const char *ln, int tablen) {
  int c, steps;
  int col = 0;
  while ((c = *ln++) != '\n') {
    if (c == '\t') {
      steps = tablen - (col % tablen);
      spacedln = entab(spacedln, steps);
      col = 0;
    } else {
      *spacedln++ = c;
      col++;
    }
  }
  *spacedln++ = '\n';
  *spacedln = '\0';
}

char *entab(char *spacedln, int steps) {
  while (steps--)
    *spacedln++ = ' ';
  return spacedln;
}

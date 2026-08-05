#include <stdio.h>

#define TABN 8

void print_spaces(int n);

int main(int argc, char *argv[]) {
  int tabn;
  tabn = TABN;

  int c;
  int col = 0;
  int spaces = 0;
  while ((c = getchar()) != EOF) {
    col++;
    if (c == ' ') {
      spaces++;
      if (col % tabn == 0) {
        printf("\t");
        spaces = 0;
      }
      continue;
    } else {
      if (c == '\n') {
        print_spaces(spaces);
        printf("%c", c);
        spaces = col = 0;
        continue;
      } else if (spaces) {
        print_spaces(spaces);
        printf("%c", c);
        spaces = 0;
        continue;
      } else
        printf("%c", c);
    }
  }
  return 0;
}

void print_spaces(int n) {
  while (n--)
    printf(" ");
}

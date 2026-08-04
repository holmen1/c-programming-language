#include <stdio.h>

#define TABN 8

void print_spaces(int n);

int main(int argc, char *argv[]) {
  int tabn;
  tabn = TABN;

  int c;
  int col = 0;
  int step;
  while ((c = getchar()) != EOF) {
    switch (c) {
    case '\n':
      printf("%c", c);
      col = 0;
      break;
    case '\t':
      step = tabn - (col % tabn);
      print_spaces(step);
      col += step;
      break;
    default:
      printf("%c", c);
      col++;
      break;
    }
  }
  return 0;
}

void print_spaces(int n) {
  while (n--)
    printf(" ");
}

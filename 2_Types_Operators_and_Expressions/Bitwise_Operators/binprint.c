#include <stdio.h>

void print_binary(int n) {
  int i;
  for (i = sizeof(int) * 8 - 1; i >= 0; i--)
    printf("%d", (n >> i) & 1);
  printf("\n");
}


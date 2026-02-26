#include "setbits.h"
#include <stdio.h>

void print_binary(int n) {
  int i;
  for (i = sizeof(int) * 8 - 1; i >= 0; i--)
    printf("%d", (n >> i) & 1);
  printf("\n");
}

int main(void) {

  /* setbits */
  unsigned x = 0b10101010;
  unsigned y = 0b11001100;
  int p = 4;
  int n = 3;

  printf("- setbits:  set n rightmost bits of y from position p\n");
  printf("p\t\t=\t%d\n", p);
  printf("n\t\t=\t%d\n", n);
  printf("Binary x\t=\t");
  print_binary(x);
  printf("Binary y\t=\t");
  print_binary(y);
  printf("setbits(x,p,n,y)=\t");
  print_binary(setbits(x, p, n, y));

  return 0;
}

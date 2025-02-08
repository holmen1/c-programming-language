#include <stdio.h>

void print_binary(int n) {
  int i;
  for (i = sizeof(int) * 8 - 1; i >= 0; i--)
    printf("%d", (n >> i) & 1);
  printf("\n");
}

/* getbits:  get n bits from position p */
unsigned getbits(unsigned x, int p, int n)
{
  return (x >> (p+1-n)) & ~(~0 << n);
}

/* setbits:  set n rightmost bits of y from position p */
unsigned setbits(unsigned x, int p, int n, unsigned y)
{
  int mask = ~(~0 << n);
  return (x & ~(mask << (p + 1 - n))) | ((y & mask) << (p + 1 - n));
}

/* setbits: explained */
unsigned setbits_steps(unsigned x, int p, int n, unsigned y)
{
  /* n rightmost 1s */
  unsigned mask = ~(~0 << n);
  /* clear n bits from position p in x */
  x = x & ~(mask << (p + 1 - n));
  /* first n y bits shifted to p to fit x above */
  y = (y & mask) << (p + 1 - n);
  return x | y;
}

int main() {
  char a = 'a';
  printf("Decimal '%c'\t=\t%d\n", a, a);
  printf("Binary '%c'\t=\t", a);
  print_binary(a);
  printf("\n");

  printf("\n- complement\n");
  int n = 77;
  printf("n\t\t=\t%d\n", n);
  printf("Binary n\t=\t");
  print_binary(n);
  printf("Binary ~n\t=\t");
  print_binary(~n);
  printf("Binary ~n+1\t=\t");
  print_binary(~n+1);
  printf("Decimal ~n+1\t=\t%d\n", ~n+1);

  printf("getbits(n,6,4)\t=\t");
  print_binary(getbits(n,6,4));

  /* setbits */
  unsigned x = 0b10101010;
  unsigned y = 0b11001100;
  int p = 4;
  n = 3;

  printf("\n- setbits\n");
  printf("Binary x\t=\t");
  print_binary(x);
  printf("Binary y\t=\t");
  print_binary(y);
  printf("p\t\t=\t%d\n", p);
  printf("n\t\t=\t%d\n", n);
  printf("setbits(x,p,n,y)=\t");
  print_binary(setbits(x,p,n,y));

  return 0;
}

/*
$ gcc -g -Wall -std=c90 bitwise.c 
$ ./a.out 
Decimal 'a'     =       97
Binary 'a'      =       00000000000000000000000001100001


- complement
n               =       77
Binary n        =       00000000000000000000000001001101
Binary ~n       =       11111111111111111111111110110010
Binary ~n+1     =       11111111111111111111111110110011
Decimal ~n+1    =       -77
getbits(n,6,4)  =       00000000000000000000000000001001

- setbits
Binary x        =       00000000000000000000000010101010
Binary y        =       00000000000000000000000011001100
p               =       4
n               =       3
setbits(x,p,n,y)=       00000000000000000000000010110010
*/

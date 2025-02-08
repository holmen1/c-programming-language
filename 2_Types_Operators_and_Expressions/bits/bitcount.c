#include <stdio.h>

void print_binary(int n) {
  int i;
  for (i = sizeof(int) * 8 - 1; i >= 0; i--)
    printf("%d", (n >> i) & 1);
  printf("\n");
}

/* bitcount: count 1 bits in x */
int bitcount(unsigned x)
{
  int b = 0;

  for (b = 0; x != 0; x >>= 1)
    if (x & 01)
      b++;
  return b;
}

/* bitcount: count 1 bits in x faster */
int bitcount2(unsigned x)
{
  int b = 0;

  while(x)
  {
    x &= (x - 1);
    b++;
  }

  return b;
}

int main() {
  unsigned x = 0b10101010;
  printf("Decimal x\t=\t%d\n", x);
  printf("Binary x\t=\t");
  print_binary(x);
  printf("bitcount(x)\t=\t%d\n", bitcount(x));

  printf("bitcount2(x)\t=\t%d\n", bitcount2(x));
  
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

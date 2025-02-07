#include <stdio.h>

void print_binary(int n) {
  int i;
  for (i = sizeof(int) * 8 - 1; i >= 0; i--)
    printf("%d", (n >> i) & 1);
  printf("\n");
}

int main() {
  char a = 'a';
  printf("Decimal %c = %d\n", a, a);
  printf("Binary %c = ", a);
  print_binary(a);
  printf("\n");

  int n = 7;
  printf("n = %d\n", n);
  printf("Binary n\t=\t");
  print_binary(n);
  printf("Binary ~n\t=\t");
  print_binary(~n);
  printf("Binary ~n+1\t=\t");
  print_binary(~n+1);
  printf("Decimal ~n+1\t=\t%d\n", ~n+1);
  return 0;
}

/*
$ gcc -g -Wall -std=c90 bitwise.c 
$ ./a.out 
Decimal a = 97
Binary a = 00000000000000000000000001100001

n = 7
Binary n        =       00000000000000000000000000000111
Binary ~n       =       11111111111111111111111111111000
Binary ~n+1     =       11111111111111111111111111111001
Decimal ~n+1    =       -7
*/

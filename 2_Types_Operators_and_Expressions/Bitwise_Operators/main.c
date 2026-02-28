#include "binprint.h"
#include "bits.h"
#include <stdio.h>

int main(void) {

  unsigned x = 0b10101010;
  unsigned y = 0b11001100;

  printf("Binary x\t=\t");
  print_binary(x);
  printf("Binary y\t=\t");
  print_binary(y);

  /* getbits */
  printf("\n- getbits:  get 4 rightmost bits of y from position 6\n");
  printf("getbits(y,6,4)=\t");
  print_binary(getbits(y, 6, 4));

  /* setbits */
  printf("\n- setbits:  set 3 rightmost bits of y from position 15\n");
  printf("setbits(x,15,3,y)=\t");
  print_binary(setbits(x, 15, 3, y));

  /* invert */
  printf("\n- invert:  invert 3 rightmost bits of x from position 7\n");
  printf("invert(x,7,3)=\t");
  print_binary(invert(x, 7, 3));

  /* rightrot:  value of integer x rotated to the right by n positions */
  printf("\n- rightrot:  x rotated to the right by 4 positions\n");
  printf("rightrot(x,4)=\t");
  print_binary(rightrot(x, 4));

  return 0;
}

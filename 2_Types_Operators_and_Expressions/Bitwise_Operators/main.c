#include "binprint.h"
#include "bitcount.h"
#include "bits.h"
#include <stdio.h>
#include <time.h>

int main(void) {

  unsigned x = 0b10101010;
  unsigned y = 0b11001100;

  printf("Binary x = ");
  print_binary(x);
  printf("Binary y = ");
  print_binary(y);

  /* getbits */
  printf("\n- getbits: get 4 rightmost bits of y from position 6\n");
  printf("getbits(y,6,4)\t\t= ");
  print_binary(getbits(y, 6, 4));

  /* setbits */
  printf("\n- setbits: set 3 rightmost bits of x from position 15\n");
  printf("setbits(y,15,3,x)\t= ");
  print_binary(setbits(y, 15, 3, x));

  /* invert */
  printf("\n- invert: invert 3 rightmost bits of x from position 7\n");
  printf("invert(x,7,3)\t\t= ");
  print_binary(invert(x, 7, 3));

  /* rightrot:  value of integer x rotated to the right by n positions */
  printf("\n- rightrot: x rotated to the right by 4 positions\n");
  printf("rightrot(x,4)\t\t= ");
  print_binary(rightrot(x, 4));

  /* BITCOUNT */
  unsigned z = 0x1440AA81; /*0b00010100010010000010101010000001;*/
  printf("\n\nBitcount versions\n\n");
  printf("Decimal z\t= %d\n", z);
  printf("Binary z\t= ");
  print_binary(z);
  printf("- bitcount(z)\t= %d\n", bitcount(z));
  printf("- bitcount2(z)\t= %d\n", bitcount2(z));
  printf("- bitcount3(z)\t= %d\n", bitcount3(z));

  /* compare the performance of bitcount, bitcount2, and bitcount3 */
  unsigned long long bitcount_executions = 0;
  unsigned long long bitcount2_executions = 0;
  unsigned long long bitcount3_executions = 0;
  const long long executions = 1E8;
  printf("\nexecutions: %llu\n", executions);

  clock_t start, end;
  double cpu_time_used;

  /* Measure time for bitcount */
  start = clock();
  int i;
  for (i = 0; i < executions; i++) {
    bitcount(z);
    bitcount_executions++;
  }
  end = clock();
  cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
  printf("bitcount time:\t%f seconds\n", cpu_time_used);

  /* Measure time for bitcount2 */
  start = clock();
  for (i = 0; i < executions; i++) {
    bitcount2(x);
    bitcount2_executions++;
  }
  end = clock();
  cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
  printf("bitcount2 time:\t%f seconds\n", cpu_time_used);

  /* Measure time for bitcount3 */
  start = clock();
  for (i = 0; i < executions; i++) {
    bitcount3(x);
    bitcount3_executions++;
  }
  end = clock();
  cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
  printf("bitcount3 time:\t%f seconds\n", cpu_time_used);

  return 0;
}
/*
bitcount: This function uses a simple loop to count the number of 1 bits by
shifting the bits of the number one by one. It is the slowest among the three
methods due to the high number of iterations and conditional checks. bitcount2:
This function uses Brian Kernighan's algorithm, which is faster than the simple
loop because it reduces the number of iterations by directly clearing the least
significant set bit in each iteration. This method is more efficient but still
involves multiple iterations and bitwise operations. bitcount3: This function
uses a lookup table to count the number of 1 bits in each byte of the integer.
By breaking the integer into bytes and using precomputed values from the lookup
table, it significantly reduces the number of operations required. This method
is the fastest because it leverages the precomputed results, minimizing the
computational overhead.
*/

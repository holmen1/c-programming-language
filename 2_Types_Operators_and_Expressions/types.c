/* Exercise 2-1 */
#include <stdio.h>
#include <limits.h>

int main()
{
  printf("Ranges\n");
  printf("char\t\t\tmin: %d\t\t\tmax: %d\n", CHAR_MIN, CHAR_MAX);
  printf("char signed\t\tmin: %d\t\t\tmax: %d\n", SCHAR_MIN, SCHAR_MAX);
  printf("char unsigned\t\ttmax: %d\n", UCHAR_MAX);

  printf("short\t\t\tmin: %d\t\t\tmax: %d\n", SHRT_MIN, SHRT_MAX);
  printf("short unsigned\t\ttmax: %d\n", USHRT_MAX);

  printf("int\t\t\tmin: %d\t\tmax: %d\n", INT_MIN, INT_MAX);
  printf("int unsigned\t\ttmax: %ud\n", UINT_MAX);

  printf("long\t\t\tmin: %ld\tmax: %ld\n", LONG_MIN, LONG_MAX);
  printf("long unsigned\t\ttmax: %lu\n", ULONG_MAX);
  return 1;
}
/*
$ gcc -g -Wall -std=c90 types.c
$ ./a.out
Ranges
char                    min: -128                       max: 127
char signed             min: -128                       max: 127
char unsigned           tmax: 255
short                   min: -32768                     max: 32767
short unsigned          tmax: 65535
int                     min: -2147483648                max: 2147483647
int unsigned            tmax: 4294967295d
long                    min: -9223372036854775808       max: 9223372036854775807
long unsigned           tmax: 18446744073709551615
*/ 

# Exercises

### 2.9 Bitwise Operators

#### Exercise 2-6

Write a function [setbits(x,p,n,y)](2_Types_Operators_and_Expressions/Bitwise_Operators/bits.c)
that returns x with the n bits
that begin at position p set to the rightmost n bits of y, leaving the other bits unchanged.

#### Exercise 2-7

Write a function [invert(x,p,n)](2_Types_Operators_and_Expressions/Bitwise_Operators/bits.c)
that returns x with the n bits that begin at position p inverted, leaving the other bits unchanged.

#### Exercise 2-8

Write a function [rightrot(x,n)](2_Types_Operators_and_Expressions/Bitwise_Operators/bits.c)
that returns the value of the integer x rotated to the right by n bit positions.

### 2.10 Assignment Operators and Expressions

#### Exercise 2-9

In a two's complement number system, ```x &= (x - 1)``` deletes the rightmost 1-bit in x.
Explain why. Use this observation to write a faster version of
[bitcount(x)](2_Types_Operators_and_Expressions/Bitwise_Operators/bitcount.c).

bitcount: This function uses a simple loop to count the number of 1 bits by shifting the bits of the number one by one. It is the slowest among the three methods due to the high number of iterations and conditional checks.  
bitcount2: This function uses Brian Kernighan's algorithm, which is faster than the simple loop because it reduces the number of iterations by directly clearing the least significant set bit in each iteration. This method is more efficient but still involves multiple iterations and bitwise operations.  
bitcount3: This function uses a lookup table to count the number of 1 bits in each byte of the integer. By breaking the integer into bytes and using precomputed values from the lookup table, it significantly reduces the number of operations required. This method is the fastest because it leverages the precomputed results, minimizing the computational overhead.

```bash
Bitwise_Operators $ gcc -Wall -O2 main.c bits.c binprint.c bitcount.c
Bitwise_Operators $ ./a.out
Binary x = 00000000 00000000 00000000 10101010
Binary y = 00000000 00000000 00000000 11001100

- getbits: get 4 rightmost bits of y from position 6
getbits(y,6,4)          = 00000000 00000000 00000000 00001001

- setbits: set 3 rightmost bits of x from position 15
setbits(y,15,3,x)       = 00000000 00000000 01000000 11001100

- invert: invert 3 rightmost bits of x from position 7
invert(x,7,3)           = 00000000 00000000 00000000 01001010

- rightrot: x rotated to the right by 4 positions
rightrot(x,4)           = 10100000 00000000 00000000 00001010


Bitcount versions

Decimal z       = 339782273
Binary z        = 00010100 01000000 10101010 10000001
- bitcount(z)   = 9
- bitcount2(z)  = 9
- bitcount3(z)  = 9

executions: 100000000
bitcount time:  1.120741 seconds
bitcount2 time: 0.287147 seconds
bitcount3 time: 0.097398 seconds
```


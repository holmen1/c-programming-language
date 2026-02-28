# Exercises

### 2.9 Bitwise Operators

#### Exercise 2-6

Write a function [setbits(x,p,n,y)](2_Types_Operators_and_Expressions/Bitwise_Operators/setbits.c)
that returns x with the n bits
that begin at position p set to the rightmost n bits of y, leaving the other bits unchanged.

#### Exercise 2-7

Write a function [invert(x,p,n)](2_Types_Operators_and_Expressions/Bitwise_Operators/invert.c)
that returns x with the n bits that begin at position p inverted, leaving the other bits unchanged.


```bash
$ ./a.out 
Binary x        =       00000000000000000000000010101010
Binary y        =       00000000000000000000000011001100

- getbits:  get 4 rightmost bits of y from position 6
getbits(y,6,4)= 00000000000000000000000000001001

- setbits:  set 3 rightmost bits of y from position 4
setbits(x,4,3,y)=       00000000000000000000000010110010
```

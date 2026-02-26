# Exercises

### 2.9 Bitwise Operators

#### Exercise 2-6

Write a function [setbits(x,p,n,y)](2_Types_Operators_and_Expressions/Bitwise_Operators/setbits.c)
that returns x with the n bits
that begin at position p set to the rightmost n bits of y, leaving the other bits unchanged.

```bash
$ ./a.out 
- setbits:  set n rightmost bits of y from position p
p               =       4
n               =       3
Binary x        =       00000000000000000000000010101010
Binary y        =       00000000000000000000000011001100
setbits(x,p,n,y)=       00000000000000000000000010110010
```

**Explanation**:
The `setbits` function replaces `n` bits in `x` starting at position `p` (0-indexed) with the rightmost `n` bits of `y`. In this example:
- `x = 0b10101010`
- `y = 0b11001100`
- `p = 4` (0-indexed)
- `n = 3`

Steps:
1. Extract the 3 rightmost bits of `y` (`0b100`).
2. Shift these bits to position `p` in `x`.
3. Clear the corresponding bits in `x` and replace them with the shifted bits from `y`.



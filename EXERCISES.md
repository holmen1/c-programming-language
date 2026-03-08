# Exercises

## Types, Operators and Expressions

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

**Optimazation effect**
```bash
$ gcc -Wall -O0 main.c bits.c binprint.c bitcount.c
$ ./a.out

executions: 100000000
bitcount time:  4.516744 seconds
bitcount2 time: 0.574642 seconds
bitcount3 time: 0.230350 seconds
```

Optimization effect (-O0 → -O2):
- bitcount:  4.52s → 1.12s  (~4.0x speedup) — largest gain; the compiler can aggressively
  unroll and simplify the shift-and-test loop since it has no inter-iteration dependencies
  other than the counter.
- bitcount2: 0.57s → 0.29s  (~2.0x speedup) — moderate gain; the `x &= (x-1)` loop has
  a strict data dependency (each iteration depends on the previous x), limiting instruction-
  level parallelism and preventing unrolling.
- bitcount3: 0.23s → 0.10s  (~2.4x speedup) — also moderate; the four table lookups are
  already independent and fast, but the compiler still benefits from better register
  allocation and inlining.

## Control Flow

### 3.5 Loops -- While and For

#### Exercise 3-3

Write a function [expand(s1,s2)](3_Control_Flow/expand.c) that expands shorthand notations
like `a-z` in the string `s1` into the equivalent complete list `abc...xyz` in `s2`.
Allow for letters of either case and digits, and be prepared to handle cases like `a-b-c`
and `a-z0-9` and `-a-z`. Arrange that a leading or trailing `-` is taken litterally.
```bash
$ ./a.out
expand '-a-z1-5-': -abcdefghijklmnopqrstuvwxyz12345-
expand 'A-Z': ABCDEFGHIJKLMNOPQRSTUVWXYZ
expand '0-9': 0123456789
expand 'a-b-c': error: 'a-b-c' is not valid
expand 'a-B': error: 'a-B' is not valid
expand '9-0': error: '9-0' is not valid
$ echo $?
1
```

### 3.5 Loops -- Do-while

#### Exercise 3-4

In a two's complement number representation,
our version of [itoa](3_Control_Flow/itoa.c) does not handle the
largest negative number, that is, the value of `n` equal to $-(2^{wordsize - 1})$.
Explain why not. Modify it to print that value correctly, regardless of the machine
on which it runs.

**Why it fails:** `itoa` uses `n = -n` to make the number positive before extracting digits.
For `INT_MIN` ($-2^{31}$ on a 32-bit `int`), the negation overflows because two's complement
is asymmetric: `|INT_MIN|` = $2^{31}$ which exceeds `INT_MAX` = $2^{31}-1$.
`-INT_MIN` overflows back to `INT_MIN` (undefined behavior). In practice this causes
`n % 10` to yield `-8`, and `-8 + '0'` produces `'('` (ASCII 40),
giving the output `-(` instead of `-2147483648`.

**Fix (`itoa2`):** Never negate `n`. Instead, extract digits with `abs(n % 10)` directly on
the negative value — `%` on a negative number gives a negative remainder in C, so `abs()`
corrects it. The sign is appended at the end as before. No overflow possible.

```bash
Word size: 64 bits
int size: 32 bits
INT_MAX: 2147483647
INT_MIN: -2147483648

Testing itoa:
32 -> 32
-64 -> -64
128 -> 128
2147483647 -> 2147483647
-2147483648 -> -(

Testing itoa2:
32 -> 32
-64 -> -64
128 -> 128
2147483647 -> 2147483647
-2147483648 -> -2147483648
```

#### Exercise 3-5

Write the function `itob(n,s,b)` that converts the integer n into a base b
character representation in the string s. In particular, `itob(n,s,16)`
formats n as a hexadecimal digit in s.

`itob` casts `n` to `unsigned int` before extracting digits, so negative values
are shown as their two's complement bit pattern — no sign character, matching the
convention of `printf("%x")` / `printf("%o")`. Prefixes follow language norms:
`0x` for hex, `0` for octal, `0b` for binary. Hex is zero-padded to 8 digits;
binary is zero-padded to 32 bits and grouped into bytes for readability.

To read the sign without a `-` symbol, inspect the most significant bit: if it
is `1` the value is negative. In hex a leading digit ≥ `8` means MSB is set
(e.g. `0xFFFFFFC0`). In binary the first bit after `0b` tells you directly
(e.g. `0b 1111...` → negative). In octal a leading digit ≥ `2` sets bit 31
(e.g. `037777777700`).

The all-ones pattern of `-64` (`0b 11111111 11111111 11111111 11000000`) is
characteristic of two's complement: negate by flipping all bits and adding 1,
so small negative numbers have almost all bits set. This is unintuitive at first
but is exactly why these representations are used — to expose the raw bit pattern
rather than hide it behind a sign.

```bash
Word size: 64 bits
int size: 32 bits
INT_MAX: 2147483647
INT_MIN: -2147483648

Testing itoa:
32 -> 32
-64 -> -64
128 -> 128
2147483647 -> 2147483647
-2147483648 -> -(

Testing itoa2:
32 -> 32
-64 -> -64
128 -> 128
2147483647 -> 2147483647
-2147483648 -> -2147483648

To hex:
32 -> 0x00000020
-64 -> 0xFFFFFFC0
128 -> 0x00000080
2147483647 -> 0x7FFFFFFF
-2147483648 -> 0x80000000

To oct:
32 -> 040
-64 -> 037777777700
128 -> 0200
2147483647 -> 017777777777
-2147483648 -> 020000000000

To bin:
32 -> 0b 00000000 00000000 00000000 00100000
-64 -> 0b 11111111 11111111 11111111 11000000
128 -> 0b 00000000 00000000 00000000 10000000
2147483647 -> 0b 01111111 11111111 11111111 11111111
-2147483648 -> 0b 10000000 00000000 00000000 00000000
```




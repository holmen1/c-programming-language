#ifndef BITS_H
#define BITS_H

/* getbits:  get n rightmost bitsfrom position p */
unsigned getbits(unsigned x, int p, int n);

/* setbits:  set n rightmost bits of y from position p */
unsigned setbits(unsigned x, int p, int n, unsigned y);

/* getbits:  get n rightmost bitsfrom position p */
unsigned invert(unsigned x, int p, int n);

/* rightrot:  value of integer x rotated to the right by n positions */
unsigned rightrot(unsigned x, int n);

#endif

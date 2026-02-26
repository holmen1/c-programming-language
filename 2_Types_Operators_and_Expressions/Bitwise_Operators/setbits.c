/* setbits:  set n rightmost bits of y from position p */
unsigned setbits(unsigned x, int p, int n, unsigned y)
{
  int mask = ~(~0 << n);
  return (x & ~(mask << (p + 1 - n))) | ((y & mask) << (p + 1 - n));
}


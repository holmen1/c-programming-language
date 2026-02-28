/* getbits:  get n rightmost bits from position p */
unsigned getbits(unsigned x, int p, int n) {
  int mask = ~(~0 << n);
  return x >> (p + 1 - n) & mask;
}

/* setbits:  set n rightmost bits of y from position p */
unsigned setbits(unsigned x, int p, int n, unsigned y) {
  int mask = ~(~0 << n);
  int shift = p + 1 - n;
  return (x & ~(mask << shift)) | getbits(y, n - 1, n) << shift;
}

/* invert:  invert n rightmost bits from position p */
unsigned invert(unsigned x, int p, int n) {
  int splice = getbits(x, p, n);
  return setbits(x, p, n, ~splice);
}

/* rightrot:  value of integer x rotated to the right by n positions */
unsigned rightrot(unsigned x, int n) {
  int end, tmp = x, i = 1;

  while (i <= n) {
    end = getbits(tmp, 0, 1);
    tmp >>= 1;
    tmp = setbits(tmp, sizeof(int) * 8 - 1, 1, end);
    i++;
  }
  return tmp;
}

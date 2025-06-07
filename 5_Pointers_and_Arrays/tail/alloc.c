#define ALLOCSIZE 100000

static char allocbuf[ALLOCSIZE]; /* storage for alloc */
static char* allocp = allocbuf; /* next free position */

char* alloc(int n) {
  if (allocbuf + ALLOCSIZE - allocp >= n) { /* it fits */
    allocp += n;
    return allocp - n; /* old p */
  } else {
    return 0;
  }
}

void afree(char* p) { /* free storage pointed to by p */
  if (p >= allocbuf && p < allocbuf + ALLOCSIZE) {
    allocp = p; /* reset allocp to p */
  }
}
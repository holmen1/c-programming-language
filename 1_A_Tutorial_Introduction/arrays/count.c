#include <stdio.h>

/* count digits, space, others */
int main()
{
  int c, i, nwhite, nother;
  int ndigit[10];

  nwhite = nother = 0;
  for (i = 0; i < 10; ++i)
    ndigit[i] = 0;

  while ((c = getchar()) != EOF)
    if (c >= '0' && c <= '9')
      ++ndigit[c-'0'];
    else if (c == ' ' || c == '\n' || c == '\t')
      ++nwhite;
    else
      ++nother;

  printf("digits = ");
  for (i = 0; i < 10; ++i)
    printf("%d ", ndigit[i]);
  printf(", blanks = %d, other = %d\n", nwhite, nother);

  return 0;
}
/*
$ gcc -std=c89 count.c -o count
$ ./count < count.c 
digits = 10 3 0 0 0 0 0 0 0 1 , blanks = 153, other = 346
*/

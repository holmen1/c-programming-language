#include <stdio.h>
#define MAXLENGTH 12

/* histogram of word lengths */
int main()
{
  int c, i, w, letters;
  int wlength[MAXLENGTH];

  letters = 0;
  for (i = 0; i < MAXLENGTH; ++i)
    wlength[i] = 0;

  while ((c = getchar()) != EOF)
    if (c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z')
      ++letters;
    else if (letters > 0) {
      ++wlength[letters];
      letters = 0;
    }

  for (i = 1; i < MAXLENGTH; ++i) {
    printf("%d: ", i);
    for (w = 0; w < wlength[i]; ++w)
      putchar('+');
    putchar('\n');
  }    

  return 0;
}
/*
$ gcc -std=c89 histogram.c -o histogram
$ ./histogram < histogram.c 
1: +++++++++++++++++++++++++++++++
2: +++
3: +++++++++
4: +++
5: +++++++
6: +++++
7: +++++++++++++++
8: 
9: +++++
10: 
*/

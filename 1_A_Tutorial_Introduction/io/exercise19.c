#include <stdio.h>


/*
copy input to output, replacing each string of one or more blanks by a single blank
*/
main()
{
  int c;
  int last_c = '\0';

  while ((c = getchar()) != EOF)
  {
    if (c != ' ' || last_c != ' ')
    {
      putchar(c);
    }

    last_c = c;
  }
}

/*
$ ./a.out 
Hello   World !
Hello World !
*/
#include <stdio.h>
/*
count blanks, tabs and newlines
*/
main()
{
  int c, specs;

  specs = 0;
  while ((c = getchar()) != EOF)
    if (c == ' ' || c == '\t' || c == '\n')
      ++specs;
  printf("Found %d special characters\n", specs);
}

/*
$ ./a.out 
abc     def ghi
Found 3 special characters
*/

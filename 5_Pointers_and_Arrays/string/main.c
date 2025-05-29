#include <stdio.h>
#include "string.h"

main ()
{
    char fst[] = "hell";
    char snd[] = "o World";

    strcat(fst, snd);
    printf("%s\n", fst);

    char end[] = "orld";
    printf("strend(fst, end) = %d\n", strend(fst, end));
    printf("strend(snd, end) = %d\n", strend(snd, end));
}

/*
$ gcc -std=c90 -Wall strcat.c main.c
strcat.c:2:6: warning: conflicting types for built-in function ‘strcat’; expected ‘char *(char *, const char *)’ [-Wbuiltin-declaration-mismatch]
    2 | void strcat(char* s, char*t)
      |      ^~~~~~
strcat.c:1:1: note: ‘strcat’ is declared in header ‘<string.h>’
  +++ |+#include <string.h>
    1 |
strcat.c: In function ‘strcat’:
strcat.c:6:16: warning: suggest parentheses around assignment used as truth value [-Wparentheses]
    6 |         while (*s++ = *t++)
      |                ^
In file included from main.c:2:
string.h:2:6: warning: conflicting types for built-in function ‘strcat’; expected ‘char *(char *, const char *)’ [-Wbuiltin-declaration-mismatch]
    2 | void strcat(char*, char*);
      |      ^~~~~~
string.h:1:1: note: ‘strcat’ is declared in header ‘<string.h>’
  +++ |+#include <string.h>
    1 |
main.c:4:1: warning: return type defaults to ‘int’ [-Wreturn-type]
    4 | main ()
      | ^~~~
main.c: In function ‘main’:
main.c:11:1: warning: control reaches end of non-void function [-Wreturn-type]
   11 | }
      | ^
holmen1@hp ~/repos/c-programming-language/5_Pointers_and_Arrays/string master* $ ./a.out
hello World
strend(fst, end) = 1
strend(snd, end) = 0
*/

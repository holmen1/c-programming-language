#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include "getword.h"

struct key {
    char *word;
    int count;
} keytab[] = {
    {"auto", 0},
    {"break", 0},
    {"case", 0},
    {"char", 0},
    {"const", 0},
    {"continue", 0},
    {"default", 0},
    {"define", 0},
    {"for", 0},
    {"if", 0},
    {"include", 0},
    {"int", 0},
    {"unsigned", 0},
    {"void", 0},
    {"while", 0}
};

#define NKEYS (sizeof keytab / sizeof(struct key))
#define MAXWORD 100

int binsearch(char *, struct key *, int);

/* count C keywords */
int main(void)
{
    int n;
    char word[MAXWORD];
    
    while (getword(word, MAXWORD) != EOF)
        if (isalpha(word[0]))
            if ((n = binsearch(word, keytab, NKEYS)) >= 0)
	        keytab[n].count++;
    for (n = 0; n < NKEYS; n++)
	if (keytab[n].count > 0)
	    printf("%4d %s\n", keytab[n].count, keytab[n].word);
    return 0;
}

/* binsearch: find word in tab[0]...tab[n-1] */
int binsearch(char *word, struct key tab[], int n)
{
    int cond;
    int low, high, mid;
    
    low = 0;
    high = n - 1;
    while (low <= high) {
        mid = (low + high) / 2;
	if ((cond = strcmp(word, tab[mid].word)) < 0)
	    high = mid - 1;
	else if (cond > 0)
	    low = mid + 1;
	else
	    return mid;
    }
    return -1;
}

/*
$ gcc -std=c90 -Wall  main.c getword.c
$ ./a.out <
main.c
   1 auto
   1 break
   1 case
   5 char
   1 const
   1 continue
   1 default
   2 for
   6 if
   1 unsigned
   2 void
   3 while
*/

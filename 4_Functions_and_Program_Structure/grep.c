//
// grep
// print each line containing pattern
#include <stdio.h>
#define MAXLINE 1000

int my_getline(char line[], int max);
int strindex(char source[], char p[]);
int strrindex(char source[], char p[]);

/* find all lines matching pattern */
int main()
{
    char line[MAXLINE];
    int found = 0;

    char pattern[] = "ould";

    while (my_getline(line, MAXLINE) > 0) {
        if (strindex(line, pattern) >= 0) {
            printf("%s", line);
            found++;
        }
    }
    return found;
}


int my_getline(char line[], int max)
{
    int c, i;

    i = 0;
    while (--max > 0 && (c = getchar()) != EOF && c != '\n')
        line[i++] = c;
    if (c == '\n')
        line[i++] = c;
    line[i] = '\0';

    return i;
}

int strindex(char source[], char p[])
{
    int i, j, k;

    for (i = 0; source[i] != '\0'; i++) {
        for (j = i, k = 0; p[k] != '\0' && source[j] == p[k]; j++, k++)
            ;
        if (k > 0 && p[k] == '\0')
            return i;
    }

    return -1;
}

/* return position of rightmost occurence of p */
int strrindex(char source[], char p[])
{
    int i, j, k;

    for (i = 0; source[i] != '\0'; i++) {
        for (j = i, k = 0; p[k] != '\0' && source[j] == p[k]; j++, k++)
            ;
        if (k > 0 && p[k] == '\0')
            return i + k - 1;
    }

    return -1;
}
/*
$ gcc -std=c90 grep.c
$ ./a.out < sample.txt
Ah Love! could you and I with Fate conspire
Would not we shatter it to bits -- and then
Re-mould it nearer to the Heart's Desire!
*/


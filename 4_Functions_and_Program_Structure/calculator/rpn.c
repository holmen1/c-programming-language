//
//
//
#include <stdio.h>
#include <assert.h>

#define BUFSIZE 100


char buf[BUFSIZE]; /* buffer for ungetch */
int bufp = 0;

int getch(void) /* get a (possibly pushed back) character */
{
    return bufp > 0 ? buf[--bufp] : getchar();
}

void ungetch(int c) /* push character back to input */
{
    if (bufp >= BUFSIZE)
        printf("ungetch: too many characters\n");
    else
        buf[bufp++] = c;
}

void test_getch_ungetch() {
    int i;

    ungetch('a');
    assert(getch() == 'a');

    // Test buffer overflow
    for (i = 0; i < BUFSIZE; i++) {
        ungetch('a');
    }
    ungetch('b'); // This should print an error message

    // Test getch with no ungetch
    assert(getch() == 'a'); // Last character pushed back
    for (i = 0; i < BUFSIZE - 1; i++) {
    }
    // Now the buffer should be empty, getch should call getchar()
    // We can't test getchar() easily without user input, so we stop here
}

int main() {
    test_getch_ungetch();
    printf("All tests passed.\n");
    return 0;
}



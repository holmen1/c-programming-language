#include <stdio.h>

int getint(int *pn);

int main(void)
{
    int n, val;

    printf("Enter integers (EOF = Ctrl+D to quit):\n");
    while ((n = getint(&val)) != EOF) {
        if (n == 0) {
            printf("Not a number. Try again:\n");
        } else {
            printf("Read integer: %d\n", val);
        }
    }
    printf("Done.\n");
    return 0;
}

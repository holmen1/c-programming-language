#include <ctype.h>
#include <stdio.h>
#define MAXLINE 1000

/* returns 1 if a-c forms a valid range: same kind, ascending, separated by '-'
 */
int is_valid(char a, char b, char c) {
  return ((islower(a) && islower(c)) || (isupper(a) && isupper(c)) ||
          (isdigit(a) && isdigit(c))) &&
         b == '-' && a < c;
}

/* expands shorthand notations like a-z in s1 into full sequences in out.
   leading and trailing '-' are treated as literals.
   returns 0 on success, 1 on invalid input. */
int expand(char *s1, char *out) {

  int a, b, c;
  int j = 0; /* current write position in out */
  for (int i = 0; s1[i] != '\0'; i++) {
    a = s1[i];
    b = s1[i + 1];
    c = s1[i + 2];
    if (is_valid(a, b, c)) {
      int k;
      for (k = j; k <= j + c - a; k++)
        out[k] = a + k - j;
      i += 2; /* skip '-' and end-of-range char */
      j = k;
    } else if (a == '-' && (i == 0 || s1[i + 1] == '\0')) {
      out[j] = a; /* leading or trailing '-' is literal */
      j++;
    } else {
      fprintf(stderr, "error: '%s' is not valid\n", s1);
      return 1;
    }
  }
  out[j] = '\0';

  return 0;
}

int main(void) {
  char s2[MAXLINE];
  char *tests[] = {"-a-z1-5-", "A-Z", "0-9", "a-b-c", "a-B", "9-0", NULL};

  /* run all tests, collect errors */
  int ret = 0;
  for (char **t = tests; *t; t++) {
    printf("expand '%s': ", *t);
    fflush(stdout);
    ret |= expand(*t, s2) || !printf("%s\n", s2);
  }
  return ret;
}

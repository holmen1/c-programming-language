#include "calc.h"
#include <stdio.h>
#include <string.h>

#define CHECK(expr)                                                            \
  do {                                                                         \
    if (!(expr)) {                                                             \
      printf("FAIL %s:%d: %s\n", __FILE__, __LINE__, #expr);                   \
      fails++;                                                                 \
    }                                                                          \
  } while (0)

static int fails = 0;

/* --- unit: stack --- */
static void test_stack(void) {
  push(1.0);
  push(2.0);
  CHECK(pop() == 2.0);
  CHECK(pop() == 1.0);
  CHECK(pop() == 0.0); /* underflow returns 0 */

  push(5.0);
  push(3.0);
  swap();
  CHECK(pop() == 5.0);
  CHECK(pop() == 3.0);

  push(9.0);
  swap();              /* only one element: diagnostic printed */
  CHECK(pop() == 9.0); /* element untouched */

  push(7.0);
  push(8.0);
  clear();
  CHECK(pop() == 0.0); /* empty after clear */
}

/* --- unit: getch/ungetch --- */
static void test_getch(void) {
  ungetch('x');
  CHECK(getch() == 'x');

  ungetch('a');
  ungetch('b'); /* buffer full: diagnostic printed, 'a' stays */
  CHECK(getch() == 'a');
}

/* --- integration: pipe expression into calculator, check first output line ---
 */
static void integration(const char *expr, const char *want) {
  char cmd[128], got[64];
  FILE *fp;

  sprintf(cmd, "echo '%s' | ./calculator 2>/dev/null", expr);
  fp = popen(cmd, "r");
  if (!fp) {
    printf("FAIL popen\n");
    fails++;
    return;
  }
  got[0] = '\0';
  fgets(got, sizeof(got), fp);
  pclose(fp);
  got[strcspn(got, "\n")] = '\0';
  if (strcmp(got, want) != 0) {
    printf("FAIL '%s': got='%s' want='%s'\n", expr, got, want);
    fails++;
  }
}

int main(void) {
  test_stack();
  test_getch();

  integration("2 3 +", "\t5");
  integration("10 4 -", "\t6");
  integration("3 4 *", "\t12");
  integration("8 2 /", "\t4");
  integration("7 3 %", "\t1");
  integration("1 0 /", "error: zero divisor");

  if (fails == 0)
    puts("All tests passed.");
  else
    printf("%d test(s) FAILED.\n", fails);
  return fails ? 1 : 0;
}

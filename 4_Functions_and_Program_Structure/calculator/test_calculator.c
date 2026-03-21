/* expose popen/pclose under strict -std=c90 (POSIX.1-2001) */
#define _POSIX_C_SOURCE 200112L
#include "calc.h"
#include <math.h>
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
  puts("stack: push/pop");
  push(1.0);
  push(2.0);
  CHECK(pop() == 2.0);
  CHECK(pop() == 1.0);
  CHECK(pop() == 0.0); /* underflow returns 0 */

  puts("stack: swap");
  push(5.0);
  push(3.0);
  swap();
  CHECK(pop() == 5.0);
  CHECK(pop() == 3.0);

  puts("stack: swap with one element");
  push(9.0);
  swap();              /* only one element: diagnostic printed */
  CHECK(pop() == 9.0); /* element untouched */

  puts("stack: clear");
  push(7.0);
  push(8.0);
  clear();
  CHECK(pop() == 0.0); /* empty after clear */

  puts("stack: peek");
  push(42.0);
  CHECK(peek() == 42.0); /* peek returns top */
  CHECK(pop() == 42.0);  /* value still on stack after peek */

  puts("stack: peek empty");
  CHECK(peek() == 0.0); /* underflow returns 0 */
}

/* --- unit: getch/ungetch --- */
static void test_getch(void) {
  puts("getch: round-trip");
  ungetch('x');
  CHECK(getch() == 'x');

  puts("getch: buffer overflow");
  ungetch('a');
  ungetch('b'); /* buffer full: diagnostic printed, 'a' stays */
  CHECK(getch() == 'a');
}

/* --- integration: pipe expression into calculator, check first output line ---
 */
static void integration(const char *expr, const char *want, int from_stderr) {
  char cmd[128], got[64];
  FILE *fp;

  sprintf(cmd,
          from_stderr ? "echo '%s' | ./calculator 2>&1 1>/dev/null"
                      : "echo '%s' | ./calculator 2>/dev/null",
          expr);
  printf("integration: %s\n", expr);
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

  integration("2 3 +", "\t5", 0);
  integration("10 4 -", "\t6", 0);
  integration("3 4 *", "\t12", 0);
  integration("8 2 /", "\t4", 0);
  integration("7 3 %", "\t1", 0);
  integration("-7 3 %", "\t-1", 0);
  integration("1 0 /", "error: zero divisor", 1);
  integration("2 3 + P", "\t5", 0); /* peek: prints top, value remains */
  integration("3 D +", "\t6", 0);   /* dup: 3 duplicated then added */
  integration("5 3 S", "\t5", 0);   /* swap: top becomes 5 after swap */
  integration("3 4 C", "\t0", 0);   /* clear: stack empty, pop returns 0 */
  integration("0.3 cos 2 pow 0.3 sin 2 pow +", "\t1", 0);
  integration("1 exp", "\t2.7182818", 0); /* e^1 */

  if (fails == 0)
    puts("All tests passed.");
  else
    printf("%d test(s) FAILED.\n", fails);
  return fails ? 1 : 0;
}

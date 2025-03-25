#define NUMBER '0'
/* stack */
void push(double);
double pop(void);
void peek(void);

int getop(char []);
int getch(void);
void ungetch(int);

/* tests */
void test_push_pop(void);
void test_peek(void);
void test_operations(void);
void test_division_by_zero(void);
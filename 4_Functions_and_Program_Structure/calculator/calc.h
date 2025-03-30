#define NUMBER '0'
/* stack */
void push(double);
double pop(void);
void peek(void);
void swap(void);
void clear(void);

int getop(char []);
int getch(void);
void ungetch(int);

/* tests */
void test_push_pop(void);
void test_peek(void);
void test_swap(void);
void test_clear(void);
void test_operations(void);
void test_division_by_zero(void);
void test_getch_ungetch();

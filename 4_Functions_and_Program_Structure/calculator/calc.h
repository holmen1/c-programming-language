#define NUMBER '0'
#define VARIABLE '1'
#define MATHOP '2'
/* stack */
void push(double);
double pop(void);
double peek(void);
void swap(void);
void clear(void);

int getop(char[]);
/* Not needed, Exercise 4-11, used in test */
int getch(void);
void ungetch(int);

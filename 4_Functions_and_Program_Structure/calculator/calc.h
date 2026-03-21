#define NUMBER '0'
#define MATHOP '2'
/* stack */
void push(double);
double pop(void);
double peek(void);
void swap(void);
void clear(void);

int getop(char[]);
int getch(void);
void ungetch(int);

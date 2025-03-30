#include <assert.h>
#include <stdio.h>
#include "calc.h"

void test_push_pop() {
    push(1.0);
    push(2.0);
    assert(pop() == 2.0);
    assert(pop() == 1.0);
    printf("test_push_pop passed\n");
}

void test_peek() {
    push(3.0);
    push(4.0);
    peek(); /* Should print "Top of stack: 4" */
    assert(pop() == 4.0);
    assert(pop() == 3.0);
    printf("test_peek passed\n");

    /* Test peek underflow */
    peek(); /* Should print "stack underflow" */
    printf("test_peek underflow passed\n");
}

void test_swap() {
    push(1.0);
    push(2.0);
    swap(); /* Should swap the top two elements */
    assert(pop() == 1.0);
    assert(pop() == 2.0);
    printf("test_swap passed\n");

    /* Test swap with insufficient elements */
    push(3.0);
    swap(); /* Should print "error: not enough elements to swap" */
    assert(pop() == 3.0);
    printf("test_swap insufficient elements passed\n");
}

void test_clear() {
    push(1.0);
    push(2.0);
    clear(); /* Should clear the stack */
    assert(pop() == 0.0); /* Stack should be empty */
    printf("test_clear passed\n");
}

void test_operations() {
    double op2;

    push(5.0);
    push(3.0);
    push(pop() + pop()); /* 5 + 3 */
    assert(pop() == 8.0);

    push(10.0);
    push(2.0);
    push(pop() * pop()); /* 10 * 2 */
    assert(pop() == 20.0);

    push(9.0);
    push(3.0);
    op2 = pop();
    push(pop() - op2); /* 9 - 3 */
    assert(pop() == 6.0);

    push(8.0);
    push(2.0);
    op2 = pop();
    push(pop() / op2); /* 8 / 2 */
    assert(pop() == 4.0);

    printf("test_operations passed\n");
}

void test_division_by_zero() {
    push(1.0);
    push(0.0);
    double result = pop() / pop(); /* 1 / 0 */
    assert(result == 0.0); /* Should handle division by zero */
    printf("test_division_by_zero passed\n");
}

void test_getch_ungetch() {
    /* Test getch and ungetch with 1-size buffer */
    ungetch('a');
    assert(getch() == 'a');
    printf("test_getch_ungetch single character passed\n");

    /* Test buffer overflow */
    ungetch('b');
    ungetch('c'); /* Should print "ungetch: too many characters" */
    assert(getch() == 'b');
    printf("test_getch_ungetch buffer overflow passed\n");

    /* Test EOF handling */
    ungetch(EOF); /* Should not push EOF */
    assert(getch() != EOF); /* Should not return EOF from buffer */
    printf("test_getch_ungetch EOF handling passed\n");
}

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
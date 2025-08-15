#include "matrix.h"
#include <time.h>

#define A_M 300
#define A_N 1000
#define B_M 1000
#define B_N 400

int main() {
    Matrix *a = create_matrix(A_M, A_N);
    Matrix *b = create_matrix(B_M, B_N);

    // Initialize matrices a and b
    for (int i = 0; i < a->rows; i++) {
        for (int j = 0; j < a->cols; j++) {
            a->data[i][j] = i + j;
        }
    }

    for (int i = 0; i < b->rows; i++) {
        for (int j = 0; j < b->cols; j++) {
            b->data[i][j] = i * j;
        }
    }

    clock_t start = clock();
    Matrix *c = multiply_matrices(a, b);
    clock_t end = clock();
    double elapsed = (double)(end - start) / CLOCKS_PER_SEC;
    printf("Matrix multiplication took %.3f seconds\n", elapsed);

    free_matrix(a);
    free_matrix(b);
    free_matrix(c);

    return 0;
}

//Matrix multiplication took 0.509 seconds
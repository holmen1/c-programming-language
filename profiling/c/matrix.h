#ifndef MATRIX_H
#define MATRIX_H

#include <stdlib.h>
#include <stdio.h>

typedef struct {
    int rows;
    int cols;
    double **data;
} Matrix;

Matrix *create_matrix(int rows, int cols);
Matrix *init_random_matrix(Matrix *m);
void free_matrix(Matrix *matrix);
Matrix *multiply_matrices(const Matrix *a, const Matrix *b);
void print_matrix(const Matrix *matrix);

#endif
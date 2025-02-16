#include <stdio.h>
#include <time.h>

/* binsearch: find x in v[0] <= v[1] <= ... <= v[n-1] */
int binsearch(int x, int v[], int n)
{
    int low, high, mid;

    low = 0;
    high = n - 1;
    while (low <= high) {
        mid = (low + high) / 2;
        if (x < v[mid])
            high = mid - 1;
        else if (x > v[mid])
            low = mid + 1;
        else    /* found match */
            return mid;
    }
    return -1;  /* no match */
}

/* optimized_binsearch: find x in v[0] <= v[1] <= ... <= v[n-1] with fewer comparisons */
int optimized_binsearch(int x, int v[], int n)
{
    int low, high, mid;

    low = 0;
    high = n - 1;
    while (low < high) {
        mid = low + (high - low >> 1);
        if (v[mid] < x)
            low = mid + 1;
        else
            high = mid;
    }
    return v[low] == x ? low : -1;
}

// Define a type for the function pointer
typedef int (*func_ptr)(int, int[], int);

// Timing function with more loops
double time_function(func_ptr f, int x, int v[], int n) {
    clock_t start, end;
    double cpu_time_used;
    int i;

    start = clock();
    for (i = 0; i < 100000000; i++) {
        f(x, v, n);
    }
    end = clock();

    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    return cpu_time_used;
}

int main()
{   int n = 100000;
    int v[n];
    for (int i = 0; i < n; i++) {
        v[i] = i;
    }
    int x = 1 + n >> 1;

    printf("Index of %d in v[] using binsearch: %d\n", x, binsearch(x, v, n));
    printf("Index of %d in v[] using optimized_binsearch: %d\n", x, optimized_binsearch(x, v, n));

    double time_taken = time_function(binsearch, x, v, n);
    printf("Time taken for binsearch: %f seconds\n", time_taken);

    time_taken = time_function(optimized_binsearch, x, v, n);
    printf("Time taken for optimized_binsearch: %f seconds\n", time_taken);

    return 0;
}
/*
Index of 50000 in v[] using binsearch: 50000
Index of 50000 in v[] using optimized_binsearch: 50000
Time taken for binsearch: 2.798556 seconds
Time taken for optimized_binsearch: 2.059559 seconds
 */
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
    while (low <= high) {
        mid = low + ((high - low) >> 1); // Use bitwise shift for division by 2
        if (v[mid] < x)
            low = mid + 1;
        else if (v[mid] > x)
            high = mid - 1;
        else
            return mid;
    }
    return -1;
}

// Define a type for the function pointer
typedef int (*func_ptr)(int, int[], int);

// Timing function with more loops
double time_function(func_ptr f, int x, int v[], int n) {
    clock_t start, end;
    double cpu_time_used;
    int i;

    start = clock();
    for (i = 0; i < 1000000; i++) {
        f(x, v, n);
    }
    end = clock();

    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    return cpu_time_used;
}

int main()
{
    int v[] = {1, 3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25, 27, 29, 31, 33, 35, 37, 39, 41, 43, 45, 47, 49, 51, 53, 55, 57, 59, 61, 63, 65, 67, 69, 71, 73, 75, 77, 79, 81, 83, 85, 87, 89, 91, 93, 95, 97, 99};
    int n = sizeof(v) / sizeof(v[0]);
    int x = 67;

    printf("Index of %d in v[] using binsearch: %d\n", x, binsearch(x, v, n));
    printf("Index of %d in v[] using optimized_binsearch: %d\n", x, optimized_binsearch(x, v, n));

    double time_taken = time_function(binsearch, x, v, n);
    printf("Time taken for binsearch: %f seconds\n", time_taken);

    time_taken = time_function(optimized_binsearch, x, v, n);
    printf("Time taken for optimized_binsearch: %f seconds\n", time_taken);

    return 0;
}
/*
Index of 67 in v[] using binsearch: 33
Index of 67 in v[] using optimized_binsearch: 33
Time taken for binsearch: 0.008253 seconds
Time taken for optimized_binsearch: 0.008280 seconds
 */
#include <stdio.h>
#include <time.h>

void print_binary(int n) {
    int i;
    for (i = sizeof(int) * 8 - 1; i >= 0; i--)
        printf("%d", (n >> i) & 1);
    printf("\n");
}

/* bitcount: count 1 bits in x */
int bitcount(unsigned x) {
    int b = 0;

    for (b = 0; x != 0; x >>= 1)
        if (x & 01)
            b++;
    return b;
}

/* bitcount2: count 1 bits in x faster */
int bitcount2(unsigned x) {
    int b = 0;

    while (x) {
        x &= (x - 1);
        b++;
    }

    return b;
}

/* Lookup table for counting bits in a byte */
static const unsigned char bit_count_table[256] = {
    0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    1, 2, 2, 3, 2, 3, 3, 4, 2, 3, 3, 4, 3, 4, 4, 5,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    2, 3, 3, 4, 3, 4, 4, 5, 3, 4, 4, 5, 4, 5, 5, 6,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    3, 4, 4, 5, 4, 5, 5, 6, 4, 5, 5, 6, 5, 6, 6, 7,
    4, 5, 5, 6, 5, 6, 6, 7, 5, 6, 6, 7, 6, 7, 7, 8
};

/* bitcount3: count 1 bits in x using lookup table */
int bitcount3(unsigned x) {
    return bit_count_table[x & 0xFF] +
           bit_count_table[(x >> 8) & 0xFF] +
           bit_count_table[(x >> 16) & 0xFF] +
           bit_count_table[(x >> 24) & 0xFF];
}


int main() {
    unsigned x = 0x1440AA81; /*0b00010100010010000010101010000001;*/
    printf("Decimal x\t\t=\t%d\n", x);
    printf("Binary x\t\t=\t");
    print_binary(x);
    printf("bitcount(x)\t\t=\t%d\n", bitcount(x));
    printf("bitcount2(x)\t=\t%d\n", bitcount2(x));
    printf("bitcount3(x)\t=\t%d\n", bitcount3(x));

    /* compare the performance of bitcount, bitcount2, and bitcount3 */
    unsigned long long bitcount_executions = 0;
    unsigned long long bitcount2_executions = 0;
    unsigned long long bitcount3_executions = 0;

    clock_t start, end;
    double cpu_time_used;

    /* Measure time for bitcount */
    start = clock();
    int i;
    for (i = 0; i < 1000000; i++) {
        bitcount(x);
        bitcount_executions++;
    }
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("bitcount executions: %llu, time: %f seconds\n", bitcount_executions, cpu_time_used);

    /* Measure time for bitcount2 */
    start = clock();
    for (i = 0; i < 1000000; i++) {
        bitcount2(x);
        bitcount2_executions++;
    }
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("bitcount2 executions: %llu, time: %f seconds\n", bitcount2_executions, cpu_time_used);

    /* Measure time for bitcount3 */
    start = clock();
    for (i = 0; i < 1000000; i++) {
        bitcount3(x);
        bitcount3_executions++;
    }
    end = clock();
    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;
    printf("bitcount3 executions: %llu, time: %f seconds\n", bitcount3_executions, cpu_time_used);

    return 0;
}

/*
$ gcc -g -Wall -std=c90 bitcount.c 
$ ./a.out 
Decimal x		=	339782273
Binary x		=	00010100010000001010101010000001
bitcount(x)		=	9
bitcount2(x)	=	9
bitcount3(x)	=	9
bitcount executions: 1000000, time: 0.044559 seconds
bitcount2 executions: 1000000, time: 0.006988 seconds
bitcount3 executions: 1000000, time: 0.001498 seconds

bitcount: This function uses a simple loop to count the number of 1 bits by shifting the bits of the number one by one. It is the slowest among the three methods due to the high number of iterations and conditional checks.
bitcount2: This function uses Brian Kernighan's algorithm, which is faster than the simple loop because it reduces the number of iterations by directly clearing the least significant set bit in each iteration. This method is more efficient but still involves multiple iterations and bitwise operations.
bitcount3: This function uses a lookup table to count the number of 1 bits in each byte of the integer. By breaking the integer into bytes and using precomputed values from the lookup table, it significantly reduces the number of operations required. This method is the fastest because it leverages the precomputed results, minimizing the computational overhead.
*/

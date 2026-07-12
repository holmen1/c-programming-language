#include <string.h>

/* qsort: sort v[left] ... v[right] in increasing order */
void qsort(char* v[], int left, int right)
{
    int i, last;
    void swap(char* v[], int i, int j);

    if (left >= right)
        return;
    swap(v, left, (left + right)/2);
    last = left;    /* move partition elem to v[0] */
    for (i = left + 1; i <= right; i++)
        if (strcmp(v[i], v[left]) < 0)
            swap(v, ++last, i);
    swap(v, left, last);
    qsort(v, left, last-1);
    qsort(v, last+1, right);
}


void swap(char* v[], int i, int j)
{
    char* tmp;

    tmp = v[i];
    v[i] = v[j];
    v[j] = tmp;
}



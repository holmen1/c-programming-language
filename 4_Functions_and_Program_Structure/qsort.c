#include <stdio.h>

void swap(int v[], int i, int j);

/* qsort: sort v[left] ... v[right] in increasing order */
void qsort(int v[], int left, int right) {
  int i, last;

  if (left >= right)
    return;
  swap(v, left, (left + right) / 2);
  last = left; /* move partition elem to v[0] */
  for (i = left + 1; i <= right; i++)
    if (v[i] < v[left])
      swap(v, ++last, i);
  swap(v, left, last);
  qsort(v, left, last - 1);
  qsort(v, last + 1, right);
}

void swap(int v[], int i, int j) {
  int tmp = v[i];
  v[i] = v[j];
  v[j] = tmp;
}

void print_array(int v[]) {
  int i;
  for (i = 0; i < 7; i++) {
    printf("%d\t", v[i]);
  }
  putchar('\n');
}

int main() {
  int sample[] = {6, 23, 1, 88, 2, 100, 3};
  print_array(sample);
  qsort(sample, 0, 6);
  print_array(sample);

  return 0;
}

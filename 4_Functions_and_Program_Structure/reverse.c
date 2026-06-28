#include <stdio.h>

void reverse_rec(char *s, int start, int end) {
  if (start >= end)
    return;
  /* Swap s[start] and s[end] */
  char temp = s[start];
  s[start] = s[end];
  s[end] = temp;
  reverse_rec(s, start + 1, end - 1);
}

void reverse(char *s) {
  int len = 0;
  while (s[len] != '\0')
    len++;
  if (len > 1)
    reverse_rec(s, 0, len - 1);
}

int main() {
  char sample[] = "hello mats";
  printf("reverse(%s) = ", sample);
  reverse(sample);
  printf("%s\n", sample);
  return 0;
}


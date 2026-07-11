/* strend: returns 1 if t occurs at end of s, 0 otherwise */
int strend(char *s, char *t) {
  char *ss = s;
  char *tt = t;

  /* Traverse to end of strings */
  while (*ss)
    ss++;
  while (*tt)
    tt++;

  /* Compare backwards, check also if s shorter than t */
  while (tt > t) {
    if (ss == s || *--ss != *--tt)
      return 0;
  }

  return 1;
}

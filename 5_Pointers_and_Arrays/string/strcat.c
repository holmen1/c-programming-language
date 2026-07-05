
void _strcat(char *s, const char *t) {
  while (*s)
    s++;
  while ((*s++ = *t++))
    ;
}

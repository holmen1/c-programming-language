/* strend: returns 1 if t occurs at end of s, 0 otherwise */
int strend(char* s, char* t)
{
    char *ss = s;
    char *tt = t;

    while (*ss) ss++;
    while (*tt) tt++;

    int len_s = ss - s;
    int len_t = tt - t;

    if (len_t > len_s)
        return 0;

    ss -= len_t;
    while (*tt && *ss++ == *t++)
        ;
    return (*tt == '\0');
}

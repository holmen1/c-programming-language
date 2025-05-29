/* strend: returns 1 if t occurs at end of s, 0 otherwise */
int strend(char* s, char* t)
{
	int n;
	char* tt = t;

	while (*tt)
		tt++;
	n = tt - t;

	while (*s)
		s++;
        
	for ( ; n > 0; n--)
		if (*s-- != *tt--)
			return 0;
	return 1;
}

// Freebsd getline.c


static ssize_t ____getdelim (char ** buf, size_t * bufsiz, int delimiter, FILE * fp)
{
	char *ptr, *eptr;


	if (*buf == NULL || *bufsiz == 0)
	{
		*bufsiz = BUFSIZ;
		if ((*buf = elektraMalloc (*bufsiz)) == NULL) return -1;
	}

	for (ptr = *buf, eptr = *buf + *bufsiz;;)
	{
		int c = fgetc (fp);
		if (c == -1)
		{
			if (feof (fp))
				return ptr == *buf ? -1 : ptr - *buf;
			else
				return -1;
		}
		*ptr++ = c;
		if (c == delimiter)
		{
			*ptr = '\0';
			return ptr - *buf;
		}
		if (ptr + 2 >= eptr)
		{
			size_t nbufsiz = *bufsiz * 2;
			ssize_t d = ptr - *buf;
			if (elektraRealloc ((void **) buf, nbufsiz) < 0) return -1;
			*bufsiz = nbufsiz;
			eptr = *buf + nbufsiz;
			ptr = *buf + d;
		}
	}
}
static ssize_t __getline (char ** buf, size_t * bufsiz, FILE * fp)
{
	return ____getdelim (buf, bufsiz, '\n', fp);
}

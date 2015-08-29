#include <stdio.h>
#include <stdlib.h>

int main(int argc, char** argv)
{
	for (int i=0; i<argc; ++i)
	{
		printf ("argv[%d]: %s\n", i, argv[i]);
	}

	char *c = getenv("algorithm");
	printf ("getenv(\"algorithm\") -> ");
	printf ("%s\n", c?c:"(null)");
	c = getenv("limit");
	printf ("getenv(\"limit\") -> ");
	printf ("%s\n", c?c:"(null)");
	return 0;
}

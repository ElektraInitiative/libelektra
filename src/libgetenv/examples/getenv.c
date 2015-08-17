#include <stdio.h>
#include <stdlib.h>

int main()
{
	char *c = getenv("algorithm");
	printf ("%s\n", c?c:"getenv algorithm not successful");
	c = getenv("limit");
	printf ("%s\n", c?c:"getenv limit not successful");
	return 0;
}

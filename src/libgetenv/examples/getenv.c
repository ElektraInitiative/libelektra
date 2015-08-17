#include <stdio.h>
#include <stdlib.h>

int main()
{
	char *c = getenv("algorithm");
	printf ("%s\n", c?c:"did not find user/sw/app/lift/algorithm");
	c = getenv("limit");
	printf ("%s\n", c?c:"did not find user/sw/app/lift/algorithm");
	return 0;
}

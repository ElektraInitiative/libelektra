/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <stdio.h>

int main (int argc, char ** argv)
{
	int i;
	for (i = 0; i < argc; ++i)
	{
		printf ("%s\n", argv[i]);
	}

	return 0;
}

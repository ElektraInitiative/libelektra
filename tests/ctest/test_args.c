/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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

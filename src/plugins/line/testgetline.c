/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <stdio.h>

int main (void)
{

	char * line = 0;
	size_t len = 0;
	FILE * fp = 0;
	getline (&line, &len, fp);
	return 0;
}

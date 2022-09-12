/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stdio.h>

int main (void)
{
	ElektraKey * k = elektraKeyNew ("user:/hello", ELEKTRA_KEY_VALUE, "Hello World", ELEKTRA_KEY_END);
	printf ("%s\n", (char *) elektraKeyValue (k));
	elektraKeyDel (k);

	return 0;
}

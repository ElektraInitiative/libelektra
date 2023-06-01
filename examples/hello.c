/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core/key.h>
#include <stdio.h>

int main (void)
{
	Key * k = keyNew ("user:/hello", KEY_VALUE, "Hello World", KEY_END);
	printf ("%s\n", (char *) keyValue (k));
	keyDel (k);

	return 0;
}

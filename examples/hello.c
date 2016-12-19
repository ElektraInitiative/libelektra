/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <kdb.h>
#include <stdio.h>

int main (void)
{
	Key * k = keyNew ("user/hello", KEY_VALUE, "Hello World", KEY_END);
	printf ("%s\n", (char *)keyValue (k));
	keyDel (k);

	return 0;
}

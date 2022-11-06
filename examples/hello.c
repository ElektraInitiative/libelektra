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
	Key * k = keyNew ("user:/hello", KEY_VALUE, "Hello World", KEY_END);
	printf ("%s\n", (char *) keyValue (k));
	keyDel (k);

	Key * k2 = keyNew ("user:/hosts/ipv6/example.com", KEY_VALUE, "2606:2800:220:1:248:1893:25c8:1946", KEY_END);

	return 0;
}

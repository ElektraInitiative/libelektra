/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/old_kdb.h>
#include <stdio.h>

void f (Key * k)
{
	printf ("\tf called with %s\n", keyName (k));
	keySetName (k, "user:/delete");
	keyDel (k);
}

void h (Key * k)
{
	printf ("\th called with %s\n", keyName (k));
	keyIncRef (k);

	f (k);

	keyDecRef (k);
}

int main (void)
{
	Key * k = keyNew ("user:/key1", KEY_END);
	printf ("key has ref %hu\n", keyGetRef (k));

	f (k);
	printf ("key is now deleted\n\n");

	k = keyNew ("user:/key2", KEY_END);
	keyIncRef (k);
	printf ("key has ref %hu\n", keyGetRef (k));

	f (k);
	printf ("key renamed to %s\n", keyName (k));

	f (k);

	keyDecRef (k);
	printf ("key has ref %hu\n", keyGetRef (k));
	keyDel (k);
	printf ("key is now deleted\n\n");

	k = keyNew ("user:/key3", KEY_END);
	printf ("key has ref %hu\n", keyGetRef (k));
	h (k);
	keyDel (k);
	printf ("key is now deleted\n");

	return 0;
}

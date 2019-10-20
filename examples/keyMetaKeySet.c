/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stddef.h>
#include <stdio.h>

void b (void)
{
	//! [Basic keyMeta]
	Key * key = keyNew ("user/test/key", KEY_END);

	keySetMeta (key, "meta1", "value1");
	keySetMeta (key, "meta2", "value2");

	// Iterate the metakeyset
	Key * cur;
	ksRewind (keyMeta (key));
	while ((cur = ksNext (keyMeta (key))) != NULL)
	{
		printf ("name: %s, value: %s\n", keyName (cur), keyString (cur));
	}

	// Directly lookup a meta key
	Key * lookupKey = ksLookupByName (keyMeta (key), "meta1", 0);
	printf ("name: %s, value: %s\n", keyName (lookupKey), keyString (lookupKey));
	keyDel (key);
	//! [Basic keyMeta]
}

int main (void)
{

	b ();

	return 0;
}

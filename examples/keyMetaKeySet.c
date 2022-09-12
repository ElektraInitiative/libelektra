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

int main (void)
{

	//! [Basic keyMeta]
	ElektraKey * key = keyNew ("user:/test/key", ELEKTRA_KEY_END);

	keySetMeta (key, "meta1", "value1");
	keySetMeta (key, "meta2", "value2");
	//! [Basic keyMeta]

	//! [Iterate keyMeta]
	ElektraKey * cur;
	ksRewind (keyMeta (key));
	while ((cur = ksNext (keyMeta (key))) != NULL)
	{
		printf ("meta name: %s, meta value: %s\n", keyName (cur), keyString (cur));
	}
	//! [Iterate keyMeta]

	//! [Lookup keyMeta]
	ElektraKey * lookupKey = ksLookupByName (keyMeta (key), "meta2", 0);
	printf ("meta name: %s, meta value: %s\n", keyName (lookupKey), keyString (lookupKey));
	keyDel (key);
	//! [Lookup keyMeta]

	return 0;
}

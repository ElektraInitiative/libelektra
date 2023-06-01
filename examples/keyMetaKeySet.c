/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <stddef.h>
#include <stdio.h>

int main (void)
{

	//! [Basic keyMeta]
	Key * key = keyNew ("user:/test/key", KEY_END);

	keySetMeta (key, "meta1", "value1");
	keySetMeta (key, "meta2", "value2");
	//! [Basic keyMeta]

	//! [Iterate keyMeta]
	Key * cur;
	KeySet * metaKeys;
	ssize_t ksSize;

	metaKeys = keyMeta (key);
	ksSize = ksGetSize (metaKeys);

	for (elektraCursor it = 0; it < ksSize; ++it)
	{
		cur = ksAtCursor (metaKeys, it);
		printf ("meta name: %s, meta value: %s\n", keyName (cur), keyString (cur));
	}
	//! [Iterate keyMeta]

	//! [Lookup keyMeta]
	Key * lookupKey = ksLookupByName (keyMeta (key), "meta2", 0);
	printf ("meta name: %s, meta value: %s\n", keyName (lookupKey), keyString (lookupKey));
	keyDel (key);
	//! [Lookup keyMeta]

	return 0;
}

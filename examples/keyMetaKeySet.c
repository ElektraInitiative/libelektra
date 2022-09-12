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
	ElektraKey * key = elektraKeyNew ("user:/test/key", ELEKTRA_KEY_END);

	elektraKeySetMeta (key, "meta1", "value1");
	elektraKeySetMeta (key, "meta2", "value2");
	//! [Basic keyMeta]

	//! [Iterate keyMeta]
	ElektraKey * cur;
	elektraKeysetRewind (elektraKeyMeta (key));
	while ((cur = elektraKeysetNext (elektraKeyMeta (key))) != NULL)
	{
		printf ("meta name: %s, meta value: %s\n", elektraKeyName (cur), elektraKeyString (cur));
	}
	//! [Iterate keyMeta]

	//! [Lookup keyMeta]
	ElektraKey * lookupKey = elektraKeysetLookupByName (elektraKeyMeta (key), "meta2", 0);
	printf ("meta name: %s, meta value: %s\n", elektraKeyName (lookupKey), elektraKeyString (lookupKey));
	elektraKeyDel (key);
	//! [Lookup keyMeta]

	return 0;
}

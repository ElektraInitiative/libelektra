/**
 * @file
 *
 * @brief some possibilites how to iterate ver a KeySet in an elegant way.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <kdbextension.h>
#include <stdio.h>

/*
 * This function demonstrates how easy it is to extend
 * elektra. We use ksNext() and keyIsBelow() to implement a function
 * which skips to the next directory. */
ElektraKey * ksNextDir (ElektraKeyset * ks)
{
	ElektraKey * cur;
	ElektraKey * startKey = ksCurrent (ks);

	if (!startKey) return (ksNext (ks));

	while ((cur = ksNext (ks)) != 0)
	{
		if (!keyIsBelow (startKey, cur)) return cur;
	}

	return 0;
}

int main (void)
{
	ElektraKey * cur = 0;
	ElektraKey * found = 0;
	ElektraKeyset * ks =
		ksNew (30, keyNew ("user:/dir1", ELEKTRA_KEY_END), keyNew ("user:/dir1/key1", ELEKTRA_KEY_VALUE, "value1", ELEKTRA_KEY_END),
		       keyNew ("user:/dir1/key2", ELEKTRA_KEY_VALUE, "value2", ELEKTRA_KEY_END), keyNew ("user:/dir1/key3", ELEKTRA_KEY_VALUE, "value3", ELEKTRA_KEY_END),
		       keyNew ("user:/dir1/key4", ELEKTRA_KEY_VALUE, "value4", ELEKTRA_KEY_END),
		       keyNew ("user:/dir1/.inactive1", ELEKTRA_KEY_COMMENT, "key is inactive", ELEKTRA_KEY_END),
		       keyNew ("user:/dir1/.inactive2", ELEKTRA_KEY_COMMENT, "additional information", ELEKTRA_KEY_END), keyNew ("user:/dir2", ELEKTRA_KEY_END),
		       keyNew ("user:/dir2/key1", ELEKTRA_KEY_VALUE, "value1", ELEKTRA_KEY_END), keyNew ("user:/dir2/key2", ELEKTRA_KEY_VALUE, "value2", ELEKTRA_KEY_END),
		       keyNew ("user:/dir2/key3", ELEKTRA_KEY_VALUE, "value3", ELEKTRA_KEY_END), keyNew ("user:/dir2/key4", ELEKTRA_KEY_VALUE, "value4", ELEKTRA_KEY_END),
		       keyNew ("user:/dir3", ELEKTRA_KEY_END), keyNew ("user:/dir3/key1", ELEKTRA_KEY_VALUE, "value1", ELEKTRA_KEY_END),
		       keyNew ("user:/dir3/.inactive1", ELEKTRA_KEY_COMMENT, "key is inactive", ELEKTRA_KEY_END),
		       keyNew ("user:/dir3/.inactive2", ELEKTRA_KEY_COMMENT, "a users comment", ELEKTRA_KEY_END), keyNew ("user:/dir4", ELEKTRA_KEY_END),
		       keyNew ("user:/dir5", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	printf ("Iterate over all keys:\n");
	ksRewind (ks);
	while ((cur = ksNext (ks)) != 0)
	{ /* Iterates over all keys and prints their name */
		printf ("%s\n", keyName (cur));
	}

	printf ("\nIterate over all directories:\n");
	ksRewind (ks);
	while ((cur = ksNextDir (ks)) != 0)
	{ /* Iterates over all keys and prints their name */
		printf ("%s\n", keyName (cur));
	}

	printf ("\nLookup and then iterate:\n");
	found = ksLookupByName (ks, "user:/dir2", 0);
	printf ("Found key %s\n", keyName (found));
	while ((cur = ksNext (ks)) != 0)
	{ /* Iterates over all keys direct below and prints their name */
		if (keyIsDirectlyBelow (found, cur) == 0) break;
		printf ("%s\n", keyName (cur));
	}

	return 0;
}

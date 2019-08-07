/**
 * @file
 *
 * @brief some possibilites how to iterate ver a KeySet in an elegant way.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <kdbextension.h>
#include <stdio.h>

/*
 * This function demonstrates how easy it is to extend
 * elektra. We use ksNext() and keyIsBelow() to implement a function
 * which skips to the next directory. */
Key * ksNextDir (KeySet * ks)
{
	Key * cur;
	Key * startKey = ksCurrent (ks);

	if (!startKey) return (ksNext (ks));

	while ((cur = ksNext (ks)) != 0)
	{
		if (!keyIsBelow (startKey, cur)) return cur;
	}

	return 0;
}

int main (void)
{
	Key * cur = 0;
	Key * found = 0;
	KeySet * ks = ksNew (
		30, keyNew ("user/dir1", KEY_DIR, KEY_END), keyNew ("user/dir1/key1", KEY_VALUE, "value1", KEY_END),
		keyNew ("user/dir1/key2", KEY_VALUE, "value2", KEY_END), keyNew ("user/dir1/key3", KEY_VALUE, "value3", KEY_END),
		keyNew ("user/dir1/key4", KEY_VALUE, "value4", KEY_END),
		keyNew ("user/dir1/.inactive1", KEY_COMMENT, "key is inactive", KEY_END),
		keyNew ("user/dir1/.inactive2", KEY_COMMENT, "additional information", KEY_END), keyNew ("user/dir2", KEY_DIR, KEY_END),
		keyNew ("user/dir2/key1", KEY_VALUE, "value1", KEY_END), keyNew ("user/dir2/key2", KEY_VALUE, "value2", KEY_END),
		keyNew ("user/dir2/key3", KEY_VALUE, "value3", KEY_END), keyNew ("user/dir2/key4", KEY_VALUE, "value4", KEY_END),
		keyNew ("user/dir3", KEY_DIR, KEY_END), keyNew ("user/dir3/key1", KEY_VALUE, "value1", KEY_END),
		keyNew ("user/dir3/.inactive1", KEY_COMMENT, "key is inactive", KEY_END),
		keyNew ("user/dir3/.inactive2", KEY_COMMENT, "a users comment", KEY_END), keyNew ("user/dir4", KEY_DIR, KEY_END),
		keyNew ("user/dir5", KEY_DIR, KEY_END), KS_END);

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
	found = ksLookupByName (ks, "user/dir2", 0);
	printf ("Found key %s\n", keyName (found));
	while ((cur = ksNext (ks)) != 0)
	{ /* Iterates over all keys direct below and prints their name */
		if (keyIsDirectBelow (found, cur) == 0) break;
		printf ("%s\n", keyName (cur));
	}

	printf ("\nIterate over inactive keys:\n");
	ksRewind (ks);
	while ((cur = ksNext (ks)) != 0)
	{ /* Iterates over inactive keys and prints their name */
		if (keyIsInactive (cur) == 0) continue;
		printf ("%s %s\n", keyName (cur), keyString (keyGetMeta (cur, "comment")));
	}

	return 0;
}

/**
 * @file
 *
 * @brief some possibilites how to iterate ver a KeySet in an elegant way.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <elektra/ease/old_ease.h>
#include <elektra/ease/meta.h>
#include <stdio.h>

/*
 * This function demonstrates how easy it is to extend
 * elektra. We use ksGetSize(), ksAtCursor() and keyIsBelow() to implement a function
 * which skips to the next directory. */
Key * ksNextDir (KeySet * ks, elektraCursor startPos)
{
	Key * cur;
	Key * startKey = NULL;
	static elektraCursor lastPos = 0;

	if (startPos >= 0)
	{
		/* use given start position */
		lastPos = startPos;
	}

	if (ksGetSize (ks) > lastPos)
	{
		startKey = ksAtCursor (ks, lastPos);
	}

	/* Check keys */
	for (lastPos = lastPos + 1; lastPos < ksGetSize (ks); ++lastPos)
	{
		cur = ksAtCursor (ks, lastPos);
		if (!keyIsBelow (startKey, cur)) return cur;
	}

	return 0;
}

int main (void)
{
	Key * cur = 0;
	Key * found = 0;
	KeySet * ks = ksNew (
		30, keyNew ("user:/dir1", KEY_END), keyNew ("user:/dir1/key1", KEY_VALUE, "value1", KEY_END),
		keyNew ("user:/dir1/key2", KEY_VALUE, "value2", KEY_END), keyNew ("user:/dir1/key3", KEY_VALUE, "value3", KEY_END),
		keyNew ("user:/dir1/key4", KEY_VALUE, "value4", KEY_END),
		keyNew ("user:/dir1/.inactive1", KEY_META, "comment/#0", "key is inactive", KEY_END),
		keyNew ("user:/dir1/.inactive2", KEY_META, "comment/#0", "additional information", KEY_END), keyNew ("user:/dir2", KEY_END),
		keyNew ("user:/dir2/key1", KEY_VALUE, "value1", KEY_END), keyNew ("user:/dir2/key2", KEY_VALUE, "value2", KEY_END),
		keyNew ("user:/dir2/key3", KEY_VALUE, "value3", KEY_END), keyNew ("user:/dir2/key4", KEY_VALUE, "value4", KEY_END),
		keyNew ("user:/dir3", KEY_END), keyNew ("user:/dir3/key1", KEY_VALUE, "value1", KEY_END),
		keyNew ("user:/dir3/.inactive1", KEY_META, "comment/#0", "key is inactive", KEY_END),
		keyNew ("user:/dir3/.inactive2", KEY_META, "comment/#0", "a users comment", KEY_END), keyNew ("user:/dir4", KEY_END),
		keyNew ("user:/dir5", KEY_END), KS_END);

	printf ("Iterate over all keys:\n");

	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{ /* Iterates over all keys and prints their name */
		cur = ksAtCursor (ks, it);
		printf ("%s\n", keyName (cur));
	}

	printf ("\nIterate over all directories:\n");


	while ((cur = ksNextDir (ks, -1)) != 0)
	{ /* Iterates over all keys and prints their name */
		printf ("%s\n", keyName (cur));
	}

	printf ("\nLookup and then iterate:\n");
	found = ksLookupByName (ks, "user:/dir2", 0);
	printf ("Found key %s\n", keyName (found));

	for (ssize_t pos = ksSearch (ks, found); pos >= 0 && pos < ksGetSize (ks); ++pos)
	{ /* Iterates over all keys direct below and prints their name */
		cur = ksAtCursor (ks, pos);
		if (keyIsDirectlyBelow (found, cur) == 0) break;
		printf ("%s\n", keyName (cur));
	}

	return 0;
}

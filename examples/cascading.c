/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/kdb.h>
#include <stdio.h>

void printError (char * what, Key const * parentKey)
{
	printf ("%s \"%s\" returned error: %s and reason %s\n", what, keyName (parentKey),
		keyString (keyGetMeta (parentKey, "error/number")), keyString (keyGetMeta (parentKey, "error/reason")));
}

int main (void)
{
	Key * parentKey = keyNew ("/", KEY_END);
	KDB * kdb = kdbOpen (NULL, parentKey);
	KeySet * ks = ksNew (0, KS_END);
	if (kdbGet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbGet", parentKey);
	}
	keyDel (parentKey);
	parentKey = keyNew ("meta:/", KEY_END);
	if (kdbGet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbGet", parentKey);
	}
	keyDel (parentKey);
	parentKey = keyNew ("/test/shell/somewhere", KEY_END);
	if (kdbGet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbGet", parentKey);
	}
	keyDel (parentKey);

	Key * k;
	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		k = ksAtCursor (ks, it);
		printf ("%s = %s\n", keyName (k), keyString (k));
	}

	/*
	k = keyNew("user:/test/shell/somewhere/key", KEY_VALUE, "value", KEY_END);
	ksAppendKey(ks, k);
	keyDel(k);
	*/

	parentKey = keyNew ("/", KEY_END);
	if (kdbSet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbSet", parentKey);
	}
	keyDel (parentKey);
	parentKey = keyNew ("meta:/", KEY_END);
	if (kdbSet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbSet", parentKey);
	}
	keyDel (parentKey);
	parentKey = keyNew ("/test/shell/somewhere", KEY_END);
	if (kdbSet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbSet", parentKey);
	}

	ksDel (ks);
	kdbClose (kdb, 0);
	keyDel (parentKey);
}

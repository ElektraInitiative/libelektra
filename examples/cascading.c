/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>

#include <stdio.h>

void printError (char * what, ElektraKey const * parentKey)
{
	printf ("%s \"%s\" returned error: %s and reason %s\n", what, keyName (parentKey),
		keyString (keyGetMeta (parentKey, "error/number")), keyString (keyGetMeta (parentKey, "error/reason")));
}

int main (void)
{
	ElektraKey * parentKey = keyNew ("/", KEY_END);
	ElektraKdb * kdb = kdbOpen (NULL, parentKey);
	ElektraKeyset * ks = ksNew (0, KS_END);
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

	ksRewind (ks);
	ElektraKey * k;
	while ((k = ksNext (ks)))
	{
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

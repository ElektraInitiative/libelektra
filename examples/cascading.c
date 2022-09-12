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
	printf ("%s \"%s\" returned error: %s and reason %s\n", what, elektraKeyName (parentKey),
		elektraKeyString (elektraKeyGetMeta (parentKey, "error/number")), elektraKeyString (elektraKeyGetMeta (parentKey, "error/reason")));
}

int main (void)
{
	ElektraKey * parentKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKdb * kdb = elektraKdbOpen (NULL, parentKey);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	if (elektraKdbGet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbGet", parentKey);
	}
	elektraKeyDel (parentKey);
	parentKey = elektraKeyNew ("meta:/", ELEKTRA_KEY_END);
	if (elektraKdbGet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbGet", parentKey);
	}
	elektraKeyDel (parentKey);
	parentKey = elektraKeyNew ("/test/shell/somewhere", ELEKTRA_KEY_END);
	if (elektraKdbGet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbGet", parentKey);
	}
	elektraKeyDel (parentKey);

	elektraKeysetRewind (ks);
	ElektraKey * k;
	while ((k = elektraKeysetNext (ks)))
	{
		printf ("%s = %s\n", elektraKeyName (k), elektraKeyString (k));
	}

	/*
	k = keyNew("user:/test/shell/somewhere/key", ELEKTRA_KEY_VALUE, "value", ELEKTRA_KEY_END);
	ksAppendKey(ks, k);
	keyDel(k);
	*/

	parentKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	if (elektraKdbSet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbSet", parentKey);
	}
	elektraKeyDel (parentKey);
	parentKey = elektraKeyNew ("meta:/", ELEKTRA_KEY_END);
	if (elektraKdbSet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbSet", parentKey);
	}
	elektraKeyDel (parentKey);
	parentKey = elektraKeyNew ("/test/shell/somewhere", ELEKTRA_KEY_END);
	if (elektraKdbSet (kdb, ks, parentKey) == -1)
	{
		printError ("kdbSet", parentKey);
	}

	elektraKeysetDel (ks);
	elektraKdbClose (kdb, 0);
	elektraKeyDel (parentKey);
}

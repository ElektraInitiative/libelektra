/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>

#include <stdio.h>

int main (void)
{
	ElektraKeyset * myConfig = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * key = elektraKeyNew ("system:/test/myapp", ELEKTRA_KEY_END);
	ElektraKdb * handle = elektraKdbOpen (NULL, key);

	elektraKdbGet (handle, myConfig, key);

	elektraKeySetName (key, "user:/test/myapp");
	elektraKdbGet (handle, myConfig, key);

	// check for errors in key
	elektraKeyDel (key);

	/*
	ksRewind(myConfig);
	while ((key = ksNext(myConfig)))
	{
		printf ("%s\n", keyName(key));
	}
	*/

	key = elektraKeysetLookupByName (myConfig, "/test/myapp/key", 0);

	// check if key is not 0 and work with it...
	if (key)
	{
		printf ("%s\n", elektraKeyString (key));
	}

	elektraKeysetDel (myConfig); // delete the in-memory configuration


	// maybe you want kdbSet() myConfig here

	elektraKdbClose (handle, 0); // no more affairs with the key database.
	return 0;
}

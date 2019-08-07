/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>

#include <stdio.h>

int main (void)
{
	KeySet * myConfig = ksNew (0, KS_END);
	Key * key = keyNew ("system/test/myapp", KEY_END);
	KDB * handle = kdbOpen (key);

	kdbGet (handle, myConfig, key);

	keySetName (key, "user/test/myapp");
	kdbGet (handle, myConfig, key);

	// check for errors in key
	keyDel (key);

	/*
	ksRewind(myConfig);
	while ((key = ksNext(myConfig)))
	{
		printf ("%s\n", keyName(key));
	}
	*/

	key = ksLookupByName (myConfig, "/test/myapp/key", 0);

	// check if key is not 0 and work with it...
	if (key)
	{
		printf ("%s\n", keyString (key));
	}

	ksDel (myConfig); // delete the in-memory configuration


	// maybe you want kdbSet() myConfig here

	kdbClose (handle, 0); // no more affairs with the key database.
	return 0;
}

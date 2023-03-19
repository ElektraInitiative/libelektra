/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <elektra/kdb/kdb.h>
#include <stdio.h>

int main (void)
{
	KeySet * config = ksNew (0, KS_END);
	Key * root = keyNew ("user:/test", KEY_END);

	printf ("Open key database\n");
	KDB * handle = kdbOpen (NULL, root);

	printf ("Retrieve key set\n");
	kdbGet (handle, config, root);

	printf ("Number of key-value pairs: %zd\n", ksGetSize (config));

	Key * key = keyNew ("user:/test/hello", KEY_VALUE, "elektra", KEY_END);
	printf ("Add key %s\n", keyName (key));
	ksAppendKey (config, key);
	printf ("Number of key-value pairs: %zd\n", ksGetSize (config));
	printf ("\n%s, %s\n\n", keyBaseName (key), keyString (key));

	// If you want to store the key database on disk, then please uncomment the following two lines
	// printf ("Write key set to disk\n");
	// kdbSet (handle, config, root);

	printf ("Delete key-value pairs inside memory\n");
	ksDel (config);
	printf ("Close key database\n");
	kdbClose (handle, 0);

	return 0;
}

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
	ElektraKeyset * config = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * root = elektraKeyNew ("user:/test", ELEKTRA_KEY_END);

	printf ("Open key database\n");
	ElektraKdb * handle = elektraKdbOpen (NULL, root);

	printf ("Retrieve key set\n");
	elektraKdbGet (handle, config, root);

	printf ("Number of key-value pairs: %zd\n", elektraKeysetGetSize (config));

	ElektraKey * key = elektraKeyNew ("user:/test/hello", ELEKTRA_KEY_VALUE, "elektra", ELEKTRA_KEY_END);
	printf ("Add key %s\n", elektraKeyName (key));
	elektraKeysetAppendKey (config, key);
	printf ("Number of key-value pairs: %zd\n", elektraKeysetGetSize (config));
	printf ("\n%s, %s\n\n", elektraKeyBaseName (key), elektraKeyString (key));

	// If you want to store the key database on disk, then please uncomment the following two lines
	// printf ("Write key set to disk\n");
	// kdbSet (handle, config, root);

	printf ("Delete key-value pairs inside memory\n");
	elektraKeysetDel (config);
	printf ("Close key database\n");
	elektraKdbClose (handle, 0);

	return 0;
}

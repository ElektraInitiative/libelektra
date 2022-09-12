/**
 * @file
 *
 * @brief example for set
 *
 * gcc -Wall -g elektra_set.c -o elektra-set `pkg-config --cflags --libs elektra`
 * Thanks to Kai-Uwe Behrmann <ku.b@gmx.de> for that example
 *
 * to clean up after executing this example you have to use:
 *
 *     kdb rm user:/sw/MyApp/Tests/TestKey1
 *
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stdio.h>

void print_warnings (ElektraKey * err)
{
	const ElektraKey * meta = 0;
	elektraKeyRewindMeta (err);
	while ((meta = elektraKeyNextMeta (err)) != 0)
	{
		printf ("%s:\t%s\n", elektraKeyName (meta), elektraKeyString (meta));
	}
}

/** After writing the key this function rereads the key and print it*/
void check_key (void)
{
	ElektraKey * error_key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKdb * kdb_handle = elektraKdbOpen (NULL, error_key);
	ElektraKey * top = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetName (top, "user:/sw/MyApp"); // == 14
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKdbGet (kdb_handle, ks, top);
	ElektraKey * key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetName (key, "user:/sw/MyApp/Tests/TestKey1"); // == 14
	ElektraKey * result = elektraKeysetLookup (ks, key, ELEKTRA_KDB_O_NONE);
	const char * key_name = elektraKeyName (result);
	const char * key_value = elektraKeyString (result);
	const char * key_comment = elektraKeyString (elektraKeyGetMeta (result, "comment"));
	printf ("key: %s value: %s comment: %s\n", key_name, key_value, key_comment);
	elektraKeysetDel (ks);
	elektraKeyDel (key);
	elektraKeyDel (top);
	elektraKdbClose (kdb_handle, error_key);
	elektraKeyDel (error_key);
}

// typical usage of Elektra
int main (void)
{
	ElektraKey * error_key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKdb * kdb_handle = elektraKdbOpen (NULL, error_key);
	ElektraKey * top = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetName (top, "user:/sw/MyApp");

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKdbGet (kdb_handle, ks, top);

	ElektraKey * key = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetName (key, "user:/sw/MyApp/Tests/TestKey1"); // == 31
	elektraKeySetString (key, "NULLTestValue");		   // == 14
	elektraKeySetMeta (key, "comment", "NULLTestComment");	   // == 16
	elektraKeysetAppendKey (ks, key);				   // == 1
	elektraKeyNeedSync (key);
	elektraKdbSet (kdb_handle, ks, top); // == -1
	print_warnings (top);
	elektraKeyDel (top);
	elektraKeysetDel (ks);
	elektraKdbClose (kdb_handle, error_key);
	elektraKeyDel (error_key);

	check_key ();

	return 0;
}

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

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/kdb.h>
#include <stdio.h>

void print_warnings (Key * err)
{
	KeySet * metaKeys = keyMeta (err);
	const Key * meta;

	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		meta = ksAtCursor (metaKeys, it);
		printf ("%s:\t%s\n", keyName (meta), keyString (meta));
	}
}

/** After writing the key this function rereads the key and print it*/
void check_key (void)
{
	Key * error_key = keyNew ("/", KEY_END);
	KDB * kdb_handle = elektraKdbOpen (NULL, error_key);
	Key * top = keyNew ("/", KEY_END);
	keySetName (top, "user:/sw/MyApp"); // == 14
	KeySet * ks = ksNew (0, KS_END);
	elektraKdbGet (kdb_handle, ks, top);
	Key * key = keyNew ("/", KEY_END);
	keySetName (key, "user:/sw/MyApp/Tests/TestKey1"); // == 14
	Key * result = ksLookup (ks, key, KDB_O_NONE);
	const char * key_name = keyName (result);
	const char * key_value = keyString (result);
	const char * key_comment = keyString (keyGetMeta (result, "comment/#0"));
	printf ("key: %s value: %s comment: %s\n", key_name, key_value, key_comment);
	ksDel (ks);
	keyDel (key);
	keyDel (top);
	elektraKdbClose (kdb_handle, error_key);
	keyDel (error_key);
}

// typical usage of Elektra
int main (void)
{
	Key * error_key = keyNew ("/", KEY_END);
	KDB * kdb_handle = elektraKdbOpen (NULL, error_key);
	Key * top = keyNew ("/", KEY_END);
	keySetName (top, "user:/sw/MyApp");

	KeySet * ks = ksNew (0, KS_END);
	elektraKdbGet (kdb_handle, ks, top);

	Key * key = keyNew ("/", KEY_END);
	keySetName (key, "user:/sw/MyApp/Tests/TestKey1"); // == 31
	keySetString (key, "NULLTestValue");		   // == 14
	keySetMeta (key, "comment/#0", "NULLTestComment"); // == 16
	ksAppendKey (ks, key);				   // == 1
	elektraKdbSet (kdb_handle, ks, top);			   // == -1
	print_warnings (top);
	keyDel (top);
	ksDel (ks);
	elektraKdbClose (kdb_handle, error_key);
	keyDel (error_key);

	check_key ();

	return 0;
}

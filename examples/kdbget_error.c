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
#include <internal/macros/utils.h> // Declares `ELEKTRA_STRINGIFY`
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printError (Key * key);
void printWarnings (Key * key);

int main (void)
{
	KeySet * myConfig = ksNew (0, KS_END);
	Key * key = keyNew ("/sw/MyApp", KEY_END);
	KDB * handle = kdbOpen (NULL, key);

	if (!handle) printError (key);


	printWarnings (key);

	if (kdbGet (handle, myConfig, key) < 0) printError (key);


	printWarnings (key);

	keyDel (key);

	// lookup
	Key * result = ksLookupByName (myConfig, "/sw/MyApp/Tests/TestKey1", 0);
	if (!result)
		printf ("Key not found in KeySet\n");
	else
	{
		// do something with the key
		const char * key_name = keyName (result);
		const char * key_value = keyString (result);
		const char * key_comment = keyString (keyGetMeta (result, "comment/#0"));
		printf ("key: %s value: %s comment: %s\n", key_name, key_value, key_comment);
	}

	ksDel (myConfig); // delete the in-memory configuration


	// maybe you want kdbSet() myConfig here

	kdbClose (handle, 0); // no more affairs with the key database.
}


/* Print and remove Error.
 * Note: not all available information will be printed!
 * Fields for more information are listed in the value from
 * the Key returned by keyGetMeta(key,"error").
 * Or print all MetaData, by using the loop from removeMetaData ().
 */
void printError (Key * key)
{
	printf ("Error occurred: %s\n", keyString (keyGetMeta (key, "error/description")));

	/*remove error*/
	Key * cutpoint = keyNew ("meta:/error", KEY_END);
	ksDel (ksCut (keyMeta (key), cutpoint));
	keyDel (cutpoint);
}


/* Check for warnings, print and remove.
 * Note: not all available information will be printed!
 * Fields for more information are listed in the value from
 * the Key returned by keyGetMeta(key,"warnings/#XX") where XX
 * is the Warning number, starting at 00.
 * Or print all MetaData, by using the loop from removeMetaData ().
 */
void printWarnings (Key * key)
{
	Key * cutpoint = keyNew ("meta:/warnings", KEY_END);
	KeySet * warnings = ksCut (keyMeta (key), cutpoint);

	for (elektraCursor i = 1; i < ksGetSize (warnings); ++i)
	{
		Key * cur = ksAtCursor (warnings, i);
		if (keyIsDirectlyBelow (cutpoint, cur))
		{
			Key * lookup = keyNew (keyName (cur), KEY_END);
			keyAddBaseName (cur, "description");
			printf ("Warning occurred: %s\n", keyString (ksLookup (warnings, lookup, KDB_O_DEL)));
		}
	}

	keyDel (cutpoint);
	ksDel (warnings);
}


/* Helper which iterates over MetaKeys from key
 * and removes all MetaKeys starting with
 * searchfor.
 */
void removeMetaData (Key * key, const char * searchfor)
{
	/* new external iterator */
	/* TODO: Test code, esp. deletion of metakeys in the iteration loop! */
	KeySet * metaKeys = keyMeta (key);
	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		Key * iter_key = ksAtCursor (metaKeys, it);
		/*startsWith*/
		if (strncmp (searchfor, keyName (iter_key), strlen (searchfor)) == 0)
		{
			if (keySetMeta (key, keyName (iter_key), 0) != 0)
			{
				printf ("Error while deleting %s\n", searchfor);
			}
		}
	}
}

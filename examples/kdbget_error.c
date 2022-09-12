/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <kdbmacros.h> // Declares `ELEKTRA_STRINGIFY`
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printError (ElektraKey * key);
void printWarnings (ElektraKey * key);

int main (void)
{
	ElektraKeyset * myConfig = ksNew (0, ELEKTRA_KS_END);
	ElektraKey * key = keyNew ("/sw/MyApp", ELEKTRA_KEY_END);
	ElektraKdb * handle = kdbOpen (NULL, key);

	if (!handle) printError (key);


	printWarnings (key);

	if (kdbGet (handle, myConfig, key) < 0) printError (key);


	printWarnings (key);

	keyDel (key);

	// lookup
	ElektraKey * result = ksLookupByName (myConfig, "/sw/MyApp/Tests/TestKey1", 0);
	if (!result)
		printf ("Key not found in KeySet\n");
	else
	{
		// do something with the key
		const char * key_name = keyName (result);
		const char * key_value = keyString (result);
		const char * key_comment = keyString (keyGetMeta (result, "comment"));
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
void printError (ElektraKey * key)
{
	printf ("Error occurred: %s\n", keyString (keyGetMeta (key, "error/description")));

	/*remove error*/
	ElektraKey * cutpoint = keyNew ("meta:/error", ELEKTRA_KEY_END);
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
void printWarnings (ElektraKey * key)
{
	ElektraKey * cutpoint = keyNew ("meta:/warnings", ELEKTRA_KEY_END);
	ElektraKeyset * warnings = ksCut (keyMeta (key), cutpoint);

	for (elektraCursor i = 1; i < ksGetSize (warnings); ++i)
	{
		ElektraKey * cur = ksAtCursor (warnings, i);
		if (keyIsDirectlyBelow (cutpoint, cur))
		{
			ElektraKey * lookup = keyNew (keyName (cur), ELEKTRA_KEY_END);
			keyAddBaseName (cur, "description");
			printf ("Warning occurred: %s\n", keyString (ksLookup (warnings, lookup, ELEKTRA_KDB_O_DEL)));
		}
	}

	keyDel (cutpoint);
	ksDel (warnings);
}


/* Helper which iterates over MetaKeys from key
 * and removes all MetaKeys starting with
 * searchfor.
 */
void removeMetaData (ElektraKey * key, const char * searchfor)
{
	const ElektraKey * iter_key;
	keyRewindMeta (key);
	while ((iter_key = keyNextMeta (key)) != 0)
	{
		/*startsWith*/
		if (strncmp (searchfor, keyName (iter_key), strlen (searchfor)) == 0)
		{
			if (keySetMeta (key, keyName (iter_key), 0) != 0) printf ("Error while deleting %s\n", searchfor);
		}
	}
}

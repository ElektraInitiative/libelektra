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
	ElektraKeyset * myConfig = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * key = elektraKeyNew ("/sw/MyApp", ELEKTRA_KEY_END);
	ElektraKdb * handle = elektraKdbOpen (NULL, key);

	if (!handle) printError (key);


	printWarnings (key);

	if (elektraKdbGet (handle, myConfig, key) < 0) printError (key);


	printWarnings (key);

	elektraKeyDel (key);

	// lookup
	ElektraKey * result = elektraKeysetLookupByName (myConfig, "/sw/MyApp/Tests/TestKey1", 0);
	if (!result)
		printf ("Key not found in KeySet\n");
	else
	{
		// do something with the key
		const char * key_name = elektraKeyName (result);
		const char * key_value = elektraKeyString (result);
		const char * key_comment = elektraKeyString (elektraKeyGetMeta (result, "comment"));
		printf ("key: %s value: %s comment: %s\n", key_name, key_value, key_comment);
	}

	elektraKeysetDel (myConfig); // delete the in-memory configuration


	// maybe you want kdbSet() myConfig here

	elektraKdbClose (handle, 0); // no more affairs with the key database.
}


/* Print and remove Error.
 * Note: not all available information will be printed!
 * Fields for more information are listed in the value from
 * the Key returned by keyGetMeta(key,"error").
 * Or print all MetaData, by using the loop from removeMetaData ().
 */
void printError (ElektraKey * key)
{
	printf ("Error occurred: %s\n", elektraKeyString (elektraKeyGetMeta (key, "error/description")));

	/*remove error*/
	ElektraKey * cutpoint = elektraKeyNew ("meta:/error", ELEKTRA_KEY_END);
	elektraKeysetDel (elektraKeysetCut (elektraKeyMeta (key), cutpoint));
	elektraKeyDel (cutpoint);
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
	ElektraKey * cutpoint = elektraKeyNew ("meta:/warnings", ELEKTRA_KEY_END);
	ElektraKeyset * warnings = elektraKeysetCut (elektraKeyMeta (key), cutpoint);

	for (elektraCursor i = 1; i < elektraKeysetGetSize (warnings); ++i)
	{
		ElektraKey * cur = elektraKeysetAtCursor (warnings, i);
		if (elektraKeyIsDirectlyBelow (cutpoint, cur))
		{
			ElektraKey * lookup = elektraKeyNew (elektraKeyName (cur), ELEKTRA_KEY_END);
			elektraKeyAddBaseName (cur, "description");
			printf ("Warning occurred: %s\n", elektraKeyString (elektraKeysetLookup (warnings, lookup, ELEKTRA_KDB_O_DEL)));
		}
	}

	elektraKeyDel (cutpoint);
	elektraKeysetDel (warnings);
}


/* Helper which iterates over MetaKeys from key
 * and removes all MetaKeys starting with
 * searchfor.
 */
void removeMetaData (ElektraKey * key, const char * searchfor)
{
	const ElektraKey * iter_key;
	elektraKeyRewindMeta (key);
	while ((iter_key = elektraKeyNextMeta (key)) != 0)
	{
		/*startsWith*/
		if (strncmp (searchfor, elektraKeyName (iter_key), strlen (searchfor)) == 0)
		{
			if (elektraKeySetMeta (key, elektraKeyName (iter_key), 0) != 0) printf ("Error while deleting %s\n", searchfor);
		}
	}
}

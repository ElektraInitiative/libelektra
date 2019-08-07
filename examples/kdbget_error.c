/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <kdbmacros.h> // Declares `ELEKTRA_STRINGIFY`
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printError (Key * key);
void printWarnings (Key * key);
void removeMetaData (Key * key, const char * searchfor);

int main (void)
{
	KeySet * myConfig = ksNew (0, KS_END);
	Key * key = keyNew ("/sw/MyApp", KEY_CASCADING_NAME, KEY_END);
	KDB * handle = kdbOpen (key);

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
void printError (Key * key)
{
	printf ("Error occurred: %s\n", keyString (keyGetMeta (key, "error/description")));

	/*remove error*/
	removeMetaData (key, "error");
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
	if (!keyGetMeta (key, "warnings")) return;
	char * end;
	size_t warn_count = strtol (keyString (keyGetMeta (key, "warnings")), &end, 10);
	if (*end)
	{
		printf ("strtol error\n");
		return;
	}
	size_t warn_iter = 0;

	char buffer[sizeof ("warnings/#00/description") + sizeof (ELEKTRA_STRINGIFY (SIZE_MAX))];

	do
	{
		snprintf (&buffer[0], sizeof (buffer), "warnings/#%02zu/description", warn_iter);

		const Key * warnkey = keyGetMeta (key, buffer);
		printf ("Warning occurred: %s\n", keyString (warnkey));
		++warn_iter;
	} while (warn_iter <= warn_count);

	/*remove all warnings*/
	removeMetaData (key, "warnings");
}


/* Helper which iterates over MetaKeys from key
 * and removes all MetaKeys starting with
 * searchfor.
 */
void removeMetaData (Key * key, const char * searchfor)
{
	const Key * iter_key;
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

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

	// for error handling see kdbget_error.c

	// clang-format off
//! [basic usage]
ElektraKey * key = elektraKeyNew ("/sw/tests/myapp/#0/current/",  ELEKTRA_KEY_END);
ElektraKdb * handle = elektraKdbOpen (NULL, key);
elektraKdbGet (handle, myConfig, key);
ElektraKey * result = elektraKeysetLookupByName (myConfig, "/sw/tests/myapp/#0/current/testkey1", 0);
//! [basic usage]
	// clang-format on

	elektraKeyDel (key);

	const char * key_name = elektraKeyName (result);
	const char * key_value = elektraKeyString (result);
	const char * key_comment = elektraKeyString (elektraKeyGetMeta (result, "comment"));
	printf ("key: %s value: %s comment: %s\n", key_name, key_value, key_comment);

	elektraKeysetDel (myConfig); // delete the in-memory configuration


	// maybe you want kdbSet() myConfig here

	elektraKdbClose (handle, 0); // no more affairs with the key database.
}

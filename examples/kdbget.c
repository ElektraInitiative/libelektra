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

	// for error handling see kdbget_error.c

	// clang-format off
//! [basic usage]
Key * key = keyNew ("/sw/tests/myapp/#0/current/",  KEY_END);
KDB * handle = kdbOpen (NULL, key);
kdbGet (handle, myConfig, key);
Key * result = ksLookupByName (myConfig, "/sw/tests/myapp/#0/current/testkey1", 0);
//! [basic usage]
	// clang-format on

	keyDel (key);

	const char * key_name = keyName (result);
	const char * key_value = keyString (result);
	const char * key_comment = keyString (keyGetMeta (result, "comment/#0"));
	printf ("key: %s value: %s comment: %s\n", key_name, key_value, key_comment);

	ksDel (myConfig); // delete the in-memory configuration


	// maybe you want kdbSet() myConfig here

	kdbClose (handle, 0); // no more affairs with the key database.
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/kdb/kdb.h>
#include <stddef.h>

int main (void)
{
	KeySet * myConfig = ksNew (0, KS_END);
	Key * parentKey = keyNew ("/sw/MyApp", KEY_END);
	KDB * handle = elektraKdbOpen (NULL, parentKey);

	elektraKdbGet (handle, myConfig, parentKey); // kdbGet() must be first
	// now any number of any kdbGet()/kdbSet() calls are allowed, e.g.:
	elektraKdbSet (handle, myConfig, parentKey);

	ksDel (myConfig); // delete the in-memory configuration

	elektraKdbClose (handle, parentKey); // no more affairs with the key database.
	keyDel (parentKey);	      // working with key/ks does not need kdb
}

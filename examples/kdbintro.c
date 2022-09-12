/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stddef.h>

int main (void)
{
	ElektraKeyset * myConfig = ksNew (0, ELEKTRA_KS_END);
	ElektraKey * parentKey = keyNew ("/sw/MyApp", ELEKTRA_KEY_END);
	ElektraKdb * handle = kdbOpen (NULL, parentKey);

	kdbGet (handle, myConfig, parentKey); // kdbGet() must be first
	// now any number of any kdbGet()/kdbSet() calls are allowed, e.g.:
	kdbSet (handle, myConfig, parentKey);

	ksDel (myConfig); // delete the in-memory configuration

	kdbClose (handle, parentKey); // no more affairs with the key database.
	keyDel (parentKey);	      // working with key/ks does not need kdb
}

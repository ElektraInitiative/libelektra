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
	ElektraKeyset * myConfig = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * parentKey = elektraKeyNew ("/sw/MyApp", ELEKTRA_KEY_END);
	ElektraKdb * handle = elektraKdbOpen (NULL, parentKey);

	elektraKdbGet (handle, myConfig, parentKey); // kdbGet() must be first
	// now any number of any kdbGet()/kdbSet() calls are allowed, e.g.:
	elektraKdbSet (handle, myConfig, parentKey);

	elektraKeysetDel (myConfig); // delete the in-memory configuration

	elektraKdbClose (handle, parentKey); // no more affairs with the key database.
	elektraKeyDel (parentKey);	      // working with key/ks does not need kdb
}

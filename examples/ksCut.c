/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>

#include <stdio.h>

void outputKeySet (ElektraKeyset * returned)
{
	elektraKeysetRewind (returned);
	while (elektraKeysetNext (returned))
	{
		printf ("%s\n", elektraKeyName (elektraKeysetCurrent (returned)));
	}
}

int main (void)
{
	// clang-format off
	//! [cut]
	ElektraKey * parentKey = elektraKeyNew ("system:/mountpoint/interest", ELEKTRA_KEY_END);
	ElektraKdb * kdb = elektraKdbOpen (NULL, parentKey);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKdbGet (kdb, ks, parentKey);
	ElektraKeyset * returned = elektraKeysetCut (ks, parentKey);
	elektraKdbSet (kdb, ks, parentKey); // all keys below cutpoint are now removed
	elektraKdbClose (kdb, parentKey);
	//! [cut]
	outputKeySet (returned);
	outputKeySet (ks);
}

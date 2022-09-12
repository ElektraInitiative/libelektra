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
	ksRewind (returned);
	while (ksNext (returned))
	{
		printf ("%s\n", keyName (ksCurrent (returned)));
	}
}

int main (void)
{
	// clang-format off
	//! [cut]
	ElektraKey * parentKey = keyNew ("system:/mountpoint/interest", ELEKTRA_KEY_END);
	ElektraKdb * kdb = kdbOpen (NULL, parentKey);
	ElektraKeyset * ks = ksNew (0, ELEKTRA_KS_END);
	kdbGet (kdb, ks, parentKey);
	ElektraKeyset * returned = ksCut (ks, parentKey);
	kdbSet (kdb, ks, parentKey); // all keys below cutpoint are now removed
	kdbClose (kdb, parentKey);
	//! [cut]
	outputKeySet (returned);
	outputKeySet (ks);
}

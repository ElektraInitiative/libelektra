/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdb.h>

#include <stdio.h>

void outputKeySet (KeySet * returned)
{
	ksRewind (returned);
	while (ksNext (returned))
	{
		printf ("%s\n", keyName (ksCurrent (returned)));
	}
}

int main ()
{
	// clang-format off
//! [cut]
Key * parentKey = keyNew ("system/mountpoint/interest", KEY_END);
KDB * kdb = kdbOpen (parentKey);
KeySet * ks = ksNew (0, KS_END);
kdbGet (kdb, ks, parentKey);
KeySet * returned = ksCut (ks, parentKey);
kdbSet (kdb, ks, parentKey); // all keys below cutpoint are now removed
kdbClose (kdb, parentKey);
//! [cut]
outputKeySet (returned);
outputKeySet (ks);
}

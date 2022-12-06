/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>

#include <stdio.h>

void outputKeySet (KeySet * returned)
{
	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		printf ("%s\n", keyName (ksAtCursor (returned, it)));
	}
}

int main (void)
{
	// clang-format off
	//! [cut]
	Key * parentKey = keyNew ("system:/mountpoint/interest", KEY_END);
	KDB * kdb = kdbOpen (NULL, parentKey);
	KeySet * ks = ksNew (0, KS_END);
	kdbGet (kdb, ks, parentKey);
	KeySet * returned = ksCut (ks, parentKey);
	kdbSet (kdb, ks, parentKey); // all keys below cutpoint are now removed
	kdbClose (kdb, parentKey);
	//! [cut]
	outputKeySet (returned);
	outputKeySet (ks);
}

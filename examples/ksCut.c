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
	KDB * kdb = elektraKdbOpen (NULL, parentKey);
	KeySet * ks = ksNew (0, KS_END);
	elektraKdbGet (kdb, ks, parentKey);
	KeySet * returned = ksCut (ks, parentKey);
	elektraKdbSet (kdb, ks, parentKey); // all keys below cutpoint are now removed
	elektraKdbClose (kdb, parentKey);
	//! [cut]
	outputKeySet (returned);
	outputKeySet (ks);
}

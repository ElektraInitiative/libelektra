/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/changetracking.h>
#include <elektra/kdb.h>

#include <stdio.h>

int main (void)
{
	Key * parentKey = keyNew ("/sample", KEY_END);
	KDB * kdb = kdbOpen (NULL, parentKey);

	KeySet * myKeySet = ksNew (0, KS_END);
	kdbGet (kdb, myKeySet, parentKey);

	ksAppendKey (myKeySet, keyNew ("user:/sample/added", KEY_END));

	const ChangeTrackingContext * ctx = elektraChangeTrackingGetContextFromKdb (kdb);
	ElektraDiff * diff = elektraChangeTrackingCalculateDiff (myKeySet, ctx, parentKey);

	KeySet * addedKeys = elektraDiffGetAddedKeys (diff);
	for (elektraCursor i = 0; i < ksGetSize (addedKeys); i++)
	{
		printf ("Added %s\n", keyName (ksAtCursor (addedKeys, i)));
	}
	ksDel (addedKeys);

	elektraDiffDel (diff);

	kdbClose (kdb, parentKey);
	ksDel (myKeySet);
	keyDel (parentKey);

	return 0;
}

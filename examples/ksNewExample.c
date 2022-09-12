/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stdio.h>

int main (void)
{
	// clang-format off
//! [Full Example]
// create a new keyset with 3 keys
// with a hint that about 20 keys will be inside
ElektraKeyset * myConfig = elektraKeysetNew (20, elektraKeyNew ("user:/name1", ELEKTRA_KEY_END), elektraKeyNew ("user:/name2", ELEKTRA_KEY_END), elektraKeyNew ("user:/name3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
// append a key in the keyset
elektraKeysetAppendKey (myConfig, elektraKeyNew ("user:/name4", ELEKTRA_KEY_END));

ElektraKey * current;
elektraKeysetRewind (myConfig);
while ((current = elektraKeysetNext (myConfig)) != 0)
{
	printf ("Key name is %s.\n", elektraKeyName (current));
}
elektraKeysetDel (myConfig); // delete keyset and all keys appended
//! [Full Example]
}

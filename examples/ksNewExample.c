/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/old_kdb.h>
#include <stdio.h>

int main (void)
{
	// clang-format off
//! [Full Example]
// create a new keyset with 3 keys
// with a hint that about 20 keys will be inside
KeySet * myConfig = ksNew (20, keyNew ("user:/name1", KEY_END), keyNew ("user:/name2", KEY_END), keyNew ("user:/name3", KEY_END), KS_END);
// append a key in the keyset
ksAppendKey (myConfig, keyNew ("user:/name4", KEY_END));

Key * current;

for (elektraCursor it = 0; it < ksGetSize (myConfig); ++it)
{
	current = ksAtCursor (myConfig, it);
	printf ("Key name is %s.\n", keyName (current));
}

ksDel (myConfig); // delete keyset and all keys appended
//! [Full Example]
}

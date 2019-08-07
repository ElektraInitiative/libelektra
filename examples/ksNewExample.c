/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <stdio.h>

int main (void)
{
	// clang-format off
//! [Full Example]
// create a new keyset with 3 keys
// with a hint that about 20 keys will be inside
KeySet * myConfig = ksNew (20, keyNew ("user/name1", 0), keyNew ("user/name2", 0), keyNew ("user/name3", 0), KS_END);
// append a key in the keyset
ksAppendKey (myConfig, keyNew ("user/name4", 0));

Key * current;
ksRewind (myConfig);
while ((current = ksNext (myConfig)) != 0)
{
	printf ("Key name is %s.\n", keyName (current));
}
ksDel (myConfig); // delete keyset and all keys appended
//! [Full Example]
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>

//! [f]
void f (KeySet * iterator, KeySet * lookup)
{
	KeySet * append = ksNew (ksGetSize (lookup), KS_END);
	Key * current;

	ksRewind (iterator);
	while ((current = ksNext (iterator)))
	{
		Key * key = ksLookup (lookup, current, KDB_O_POP);
		// do something...
		ksAppendKey (append, key); // now append it to append, not lookup!
		keyDel (key);		   // make sure to ALWAYS delete poped keys.
	}
	ksAppend (lookup, append);
	// now lookup needs to be sorted only once, append never
	ksDel (append);
}
//! [f]

int main (void)
{
	KeySet * ks1 = ksNew (20, KS_END);
	KeySet * ks2 = ksNew (20, KS_END);
	f (ks1, ks2);
}

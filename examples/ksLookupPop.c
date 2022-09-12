/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>

//! [f]
void f (ElektraKeyset * iterator, ElektraKeyset * lookup)
{
	ElektraKeyset * append = ksNew (ksGetSize (lookup), ELEKTRA_KS_END);
	ElektraKey * current;

	ksRewind (iterator);
	while ((current = ksNext (iterator)))
	{
		ElektraKey * key = ksLookup (lookup, current, ELEKTRA_KDB_O_POP);
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
	ElektraKeyset * ks1 = ksNew (20, ELEKTRA_KS_END);
	ElektraKeyset * ks2 = ksNew (20, ELEKTRA_KS_END);
	f (ks1, ks2);
}

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
	ElektraKeyset * append = elektraKeysetNew (elektraKeysetGetSize (lookup), ELEKTRA_KS_END);
	ElektraKey * current;

	elektraKeysetRewind (iterator);
	while ((current = elektraKeysetNext (iterator)))
	{
		ElektraKey * key = elektraKeysetLookup (lookup, current, ELEKTRA_KDB_O_POP);
		// do something...
		elektraKeysetAppendKey (append, key); // now append it to append, not lookup!
		elektraKeyDel (key);		   // make sure to ALWAYS delete poped keys.
	}
	elektraKeysetAppend (lookup, append);
	// now lookup needs to be sorted only once, append never
	elektraKeysetDel (append);
}
//! [f]

int main (void)
{
	ElektraKeyset * ks1 = elektraKeysetNew (20, ELEKTRA_KS_END);
	ElektraKeyset * ks2 = elektraKeysetNew (20, ELEKTRA_KS_END);
	f (ks1, ks2);
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>

ElektraKey * copy;

//! [Basic Copy All]
void l (ElektraKey * k)
{
	// receive copy
	keyCopyAllMeta (k, copy);
	// the caller will see the changed key k
	// with all the metadata from copy
}
//! [Basic Copy All]

int needsSharedData (ElektraKey * k)
{
	return k ? 1 : 0;
}

//! [Shared Meta All]
void o (ElektraKeyset * ks)
{
	ElektraKey * current;
	ElektraKey * shared = keyNew ("/", ELEKTRA_KEY_END);
	keySetMeta (shared, "shared1", "this metadata should be shared among many keys");
	keySetMeta (shared, "shared2", "this metadata should be shared among many keys also");
	keySetMeta (shared, "shared3", "this metadata should be shared among many keys too");

	ksRewind (ks);
	while ((current = ksNext (ks)) != 0)
	{
		if (needsSharedData (current)) keyCopyAllMeta (current, shared);
	}

	keyDel (shared);
}
//! [Shared Meta All]

int main (void)
{
	ElektraKey * k = keyNew ("user:/key", ELEKTRA_KEY_END);
	copy = keyNew ("user:/copy", ELEKTRA_KEY_END);
	l (k);
	keyDel (k);
	keyDel (copy);

	ElektraKeyset * ks = ksNew (20, ELEKTRA_KS_END);
	o (ks);
	ksDel (ks);
}

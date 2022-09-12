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
	elektraKeyCopyAllMeta (k, copy);
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
	ElektraKey * shared = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektraKeySetMeta (shared, "shared1", "this metadata should be shared among many keys");
	elektraKeySetMeta (shared, "shared2", "this metadata should be shared among many keys also");
	elektraKeySetMeta (shared, "shared3", "this metadata should be shared among many keys too");

	elektraKeysetRewind (ks);
	while ((current = elektraKeysetNext (ks)) != 0)
	{
		if (needsSharedData (current)) elektraKeyCopyAllMeta (current, shared);
	}

	elektraKeyDel (shared);
}
//! [Shared Meta All]

int main (void)
{
	ElektraKey * k = elektraKeyNew ("user:/key", ELEKTRA_KEY_END);
	copy = elektraKeyNew ("user:/copy", ELEKTRA_KEY_END);
	l (k);
	elektraKeyDel (k);
	elektraKeyDel (copy);

	ElektraKeyset * ks = elektraKeysetNew (20, ELEKTRA_KS_END);
	o (ks);
	elektraKeysetDel (ks);
}

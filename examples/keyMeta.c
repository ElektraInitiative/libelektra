/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
Key * copy;

//! [Basic Copy All]
void l (Key * k)
{
	// receive copy
	keyCopyAllMeta (k, copy);
	// the caller will see the changed key k
	// with all the metadata from copy
}
//! [Basic Copy All]

int needsSharedData (Key * k)
{
	return k ? 1 : 0;
}

//! [Shared Meta All]
void o (KeySet * ks)
{
	Key * current;
	Key * shared = keyNew ("/", KEY_END);
	keySetMeta (shared, "shared1", "this metadata should be shared among many keys");
	keySetMeta (shared, "shared2", "this metadata should be shared among many keys also");
	keySetMeta (shared, "shared3", "this metadata should be shared among many keys too");

	for (elektraCursor it = 0; it < ksGetSize (ks); ++it)
	{
		current = ksAtCursor (ks, it);
		if (needsSharedData (current)) keyCopyAllMeta (current, shared);
	}

	keyDel (shared);
}
//! [Shared Meta All]

int main (void)
{
	Key * k = keyNew ("user:/key", KEY_END);
	copy = keyNew ("user:/copy", KEY_END);
	l (k);
	keyDel (k);
	keyDel (copy);

	KeySet * ks = ksNew (20, KS_END);
	o (ks);
	ksDel (ks);
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdb.h>

Key *c;

//! [Basic Copy All]
void l(Key *k)
{
	// receive c
	keyCopyAllMeta(k, c);
	// the caller will see the changed key k
	// with all the metadata from c
}
//! [Basic Copy All]

int needsSharedData(Key *k)
{
	return k ? 1 : 0;
}

//! [Shared Meta All]
void o(KeySet *ks)
{
	Key *current;
	Key *shared = keyNew (0);
	keySetMeta(shared, "shared1", "this meta data should be shared among many keys");
	keySetMeta(shared, "shared2", "this meta data should be shared among many keys also");
	keySetMeta(shared, "shared3", "this meta data should be shared among many keys too");

	ksRewind(ks);
	while ((current = ksNext(ks)) != 0)
	{
		if (needsSharedData(current)) keyCopyAllMeta(current, shared);
	}

	keyDel(shared);
}
//! [Shared Meta All]

int main()
{
	Key * k = keyNew("user/key", KEY_END);
	c = keyNew("user/copy", KEY_END);
	l(k);
	keyDel(k);
	keyDel(c);

	KeySet *ks = ksNew(20, KS_END);
	o(ks);
	ksDel(ks);
}

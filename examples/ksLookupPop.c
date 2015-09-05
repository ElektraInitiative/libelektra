#include <kdb.h>

//! [f]
void f(KeySet *iterator, KeySet *lookup)
{
	KeySet *append = ksNew (ksGetSize(lookup), KS_END);
	Key *key;
	Key *current;

	ksRewind(iterator);
	while ((current=ksNext(iterator)))
	{
		key = ksLookup (lookup, current, KDB_O_POP);
		// do something...
		ksAppendKey(append, key); // now append it to append, not lookup!
		keyDel (key); // make sure to ALWAYS delete poped keys.
	}
	ksAppend(lookup, append);
	// now lookup needs to be sorted only once, append never
	ksDel (append);
}
//! [f]

int main()
{
	KeySet *ks1 = ksNew(20, KS_END);
	KeySet *ks2 = ksNew(20, KS_END);
	f(ks1, ks2);
}

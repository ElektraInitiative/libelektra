#include "public.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

int main (void)
{
	// Create key (Note: better ways below via additional APIs)
	ElektraEntry * k1 = ElektraEntryNew (&(ElektraName){ .ns = ELEKTRA_NS_SYSTEM, .name = "foo\0bar\0baz", .size = 14 });

	// get name from key
	const ElektraName * name1 = ElektraEntryGetName (k1);

	// create local name
	ElektraName name2 = { .ns = ELEKTRA_NS_USER, .name = "foo", .size = 4 };

	// ... using existing name
	ElektraEntry * k2 = ElektraEntryNew (&name2);

	// manipulate name
	ElektraNamePushPart (&name2, "boo");
	ElektraEntry * k3 = ElektraEntryNew (&name2);


	// manipulate name from key (needs copy)
	ElektraName name1Copy = { .ns = name1->ns, .name = malloc (name1->size), .size = name1->size };
	memcpy ((char *) name1Copy.name, name1->name, name1->size);
	ElektraNamePopPart (&name1Copy);
	ElektraEntry * k4 = ElektraEntryNew (&name1Copy);

	ElektraEntry *k5, *k6, *k7, *k8;

	// cleanup name copy
	free ((char *) name1Copy.name);
	name1Copy.name = NULL;
	name1Copy.size = 0;

	// Create keyset and insert keys (Note: better ways below via additional APIs)
	ElektraSet * ks1 = ElektraSetNew (8);

	ElektraSet * ks2;

	ElektraSetInsert (ks1, k1);	      // creates new reference to k1 inside ks1
	ElektraSetInsertAndRelease (ks1, k2); // moves reference from k2 into ks1, k2 must no longer be

	ElektraSetInsertAndRelease (ks1, k3);
	ElektraSetInsertAndRelease (ks1, k4);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wuninitialized"
	ElektraSetInsertAndRelease (ks1, k5);
	ElektraSetInsertAndRelease (ks1, k6);
	ElektraSetInsertAndRelease (ks1, k7);
	ElektraSetInsertAndRelease (ks1, k8);

	// copy keys from one keyset (ks2) to another (ks1)
	ElektraSetInsertAll (ks1, ks2);

	// give up reference to keyset and free memory if last reference
	ElektraSetRelease (ks2);
	// we must now stop using ks2
#pragma GCC diagnostic pop

	// lookup key inside keyset, with name from existing key ...
	__attribute__ ((unused)) ElektraEntry * l1 = ElektraSetGet (ks1, ElektraSetLookup (ks1, ElektraEntryGetName (k1)));

	// with local name ...
	// Note: Using ElektraEntryGetName (k2) would also be unsafe, because we've given up that reference
	ElektraEntry * l2 = ElektraEntryRetain (ElektraSetGet (ks1, ElektraSetLookup (ks1, &name2)));
	// or inline name
	ElektraEntry * l3 =
		ElektraSetGet (ks1, ElektraSetLookup (ks1, &(ElektraName){ .ns = ELEKTRA_NS_CASCADING, .name = "foo", .size = 4 }));
	// cascading lookup returns same key
	assert (l2 == l3);

	// give up reference to key
	ElektraEntryRelease (k1);
	// we must now stop using k1 & name1 (because name1 is tied to k1)
	// using l1 & l3 is still okay, because that reference is tied to ks1, until we ElektraSetRelease (ks1), we can still use l1 & l3
	// using l2 is fine for even longer, because we called ElektraEntryRetain, we retained our own reference
	// until we call both ElektraSetRelease (ks1) AND ElektraEntryRelease (l2), we can still use l2

	// give up additional reference to key
	ElektraEntryRelease (l2);
	// we can still use l2, but the lifetime is now tied to ks1 again

	// create new reference to key
	ElektraEntryRetain (l3);

	// give up reference to keyset
	ElektraSetRelease (ks1);
	// we must now stop using l1 & l2
	// we can still use l3, because we retained our own reference

	return 0;
}

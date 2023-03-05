#include "public.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

int main (void)
{
	// Create key (Note: better ways below via additional APIs)
	ElektraEntry * k1 = elektraEntryNew (&(ElektraName){ .ns = ELEKTRA_NS_SYSTEM, .name = "foo\0bar\0baz", .size = 14 });

	// get name from key
	const ElektraName * name1 = elektraEntryGetName (k1);

	// create local name
	ElektraName name2 = { .ns = ELEKTRA_NS_USER, .name = "foo", .size = 4 };

	// ... using existing name
	ElektraEntry * k2 = elektraEntryNew (&name2);

	// manipulate name
	elektraNamePushPart (&name2, "boo");
	ElektraEntry * k3 = elektraEntryNew (&name2);


	// manipulate name from key (needs copy)
	ElektraName name1Copy = { .ns = name1->ns, .name = malloc (name1->size), .size = name1->size };
	memcpy ((char *) name1Copy.name, name1->name, name1->size);
	elektraNamePopPart (&name1Copy);
	ElektraEntry * k4 = elektraEntryNew (&name1Copy);

	ElektraEntry *k5, *k6, *k7, *k8;

	// cleanup name copy
	free ((char *) name1Copy.name);
	name1Copy.name = NULL;
	name1Copy.size = 0;

	// Create keyset and insert keys (Note: better ways below via additional APIs)
	ElektraSet * ks1 = elektraSetNew (8);

	ElektraSet * ks2;

	elektraSetInsert (ks1, (ElektraEntry *) elektraEntryIncRefCount (k1)); // create new reference to keep k1 separate from ks1
	elektraSetInsert (ks1, k2);					       // moves reference from k2 into ks1, k2 must no longer be used

	elektraSetInsert (ks1, k3);
	elektraSetInsert (ks1, k4);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wuninitialized"
	elektraSetInsert (ks1, k5);
	elektraSetInsert (ks1, k6);
	elektraSetInsert (ks1, k7);
	elektraSetInsert (ks1, k8);

	// copy keys from one keyset (ks2) to another (ks1)
	elektraSetInsertAll (ks1, ks2);

	// destroy ks2, if there are no references
	elektraSetDel (ks2);
	// we must now stop using ks2
#pragma GCC diagnostic pop

	// lookup key inside keyset, with name from existing key ...
	__attribute__ ((unused)) ElektraEntry * l1 = elektraSetGet (ks1, elektraSetLookup (ks1, elektraEntryGetName (k1)));

	// ... with local name ...
	// Note: Using ElektraEntryGetName (k2) would also be unsafe, because we've given up that reference
	ElektraEntry * l2 = (ElektraEntry *) elektraEntryIncRefCount (elektraSetGet (ks1, elektraSetLookup (ks1, &name2)));
	// ... or inline name
	ElektraEntry * l3 =
		elektraSetGet (ks1, elektraSetLookup (ks1, &(ElektraName){ .ns = ELEKTRA_NS_CASCADING, .name = "foo", .size = 4 }));
	// cascading lookup returns same key
	assert (l2 == l3);

	// give up reference to key and destroy k1, if there are no other references
	elektraEntryDel (elektraEntryDecRefCount (k1));
	// we must now stop using k1 & name1 (because name1 is tied to k1)
	// using l1 & l3 is still okay, because that reference is tied to ks1, until we elektraSetDel (ks1) or change the contents of ks1,
	// we can still use l1 & l3
	// using l2 is fine for even longer, because we called elektraEntryIncRefCount, we created our own
	// reference until we call elektraEntryDel (elektraEntryDecRefCount (l2)), we can still use l2

	// give up additional reference to key
	elektraEntryDel (elektraEntryDecRefCount (l2));
	// we can still use l2, but the lieftime is now tied to ks1 again
	// we can use as long as ks1 or until we modify ks1

	// create new reference to key
	elektraEntryIncRefCount (l3);

	// destroy ks1, if there are no references
	elektraSetDel (ks1);
	// we must now stop using l1 & l2
	// we can still use l3, because we created our own reference

	// give up reference and destroy l3, if there are no other references
	elektraEntryDel (elektraEntryDecRefCount (l3));

	return 0;
}

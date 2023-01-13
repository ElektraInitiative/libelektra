#include "public.h"

#include <assert.h>
#include <stdlib.h>
#include <string.h>

int main (void)
{
	// Create key (Note: better ways below via additional APIs)
	ElektraKey * k1 = elektraKeyNew (&(ElektraKeyname){ .ns = ELEKTRA_NS_SYSTEM, .name = "foo\0bar\0baz", .size = 14 });

	// get name from key
	const ElektraKeyname * name1 = elektraKeyGetName (k1);

	// create local name
	ElektraKeyname name2 = { .ns = ELEKTRA_NS_USER, .name = "foo", .size = 4 };

	// ... using existing name
	ElektraKey * k2 = elektraKeyNew (&name2);

	// manipulate name
	elektraKeynamePushPart (&name2, "boo");
	ElektraKey * k3 = elektraKeyNew (&name2);


	// manipulate name from key (needs copy)
	ElektraKeyname name1Copy = { .ns = name1->ns, .name = malloc (name1->size), .size = name1->size };
	memcpy ((char *) name1Copy.name, name1->name, name1->size);
	elektraKeynamePopPart (&name1Copy);
	ElektraKey * k4 = elektraKeyNew (&name1Copy);

	ElektraKey *k5, *k6, *k7, *k8;

	// cleanup name copy
	free ((char *) name1Copy.name);
	name1Copy.name = NULL;
	name1Copy.size = 0;

	// Create keyset and insert keys (Note: better ways below via additional APIs)
	ElektraKeyset * ks1 = elektraKeysetNew (8);

	ElektraKeyset * ks2;

	elektraKeysetInsert (ks1, k1);		 // creates new reference to k1 inside ks1
	elektraKeysetInsertAndRelease (ks1, k2); // moves reference from k2 into ks1, k2 must no longer be

	elektraKeysetInsertAndRelease (ks1, k3);
	elektraKeysetInsertAndRelease (ks1, k4);

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wuninitialized"
	elektraKeysetInsertAndRelease (ks1, k5);
	elektraKeysetInsertAndRelease (ks1, k6);
	elektraKeysetInsertAndRelease (ks1, k7);
	elektraKeysetInsertAndRelease (ks1, k8);

	// copy keys from one keyset (ks2) to another (ks1)
	elektraKeysetInsertAll (ks1, ks2);

	// give up reference to keyset and free memory if last reference
	elektraKeysetRelease (ks2);
	// we must now stop using ks2
#pragma GCC diagnostic pop

	// lookup key inside keyset, with name from existing key ...
	__attribute__ ((unused)) ElektraKey * l1 = elektraKeysetGet (ks1, elektraKeysetLookup (ks1, elektraKeyGetName (k1)));

	// with local name ...
	// Note: Using elektraKeyGetName (k2) would also be unsafe, because we've given up that reference
	ElektraKey * l2 = elektraKeyRetain (elektraKeysetGet (ks1, elektraKeysetLookup (ks1, &name2)));
	// or inline name
	ElektraKey * l3 = elektraKeysetGet (
		ks1, elektraKeysetLookup (ks1, &(ElektraKeyname){ .ns = ELEKTRA_NS_CASCADING, .name = "foo", .size = 4 }));
	// cascading lookup returns same key
	assert (l2 == l3);

	// give up reference to key
	elektraKeyRelease (k1);
	// we must now stop using k1 & name1 (because name1 is tied to k1)
	// using l1 & l3 is still okay, because that reference is tied to ks1, until we elektraKeysetRelease (ks1), we can still use l1 & l3
	// using l2 is fine for even longer, because we called elektraKeyRetain, we retained our own reference
	// until we call both elektraKeysetRelease (ks1) AND elektraKeyRelease (l2), we can still use l2

	// give up additional reference to key
	elektraKeyRelease (l2);
	// we can still use l2, but the lifetime is now tied to ks1 again

	// create new reference to key
	elektraKeyRetain (l3);

	// give up reference to keyset
	elektraKeysetRelease (ks1);
	// we must now stop using l1 & l2
	// we can still use l3, because we retained our own reference

	return 0;
}

#include "public.h"

int main (void)
{
	int a = 7;

	ElektraEntry * key = elektraEntryNew (ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo")); // refs = 0

	ElektraSet * stringMeta = ELEKTRA_KEYSET (elektraEntryBuildMeta (ELEKTRA_METANAME ("type"), ELEKTRA_KEYVALUE_STRING ("string")));
	// increment refcount so we can use it more than once
	// Note: could also be inlined on first use (but only on first use)
	elektraSetIncRefCount (stringMeta);

	ElektraSet * ks = ELEKTRA_KEYSET (
		// we call elektraEntryIncRefCount, so we can keep our own reference
		(ElektraEntry *) elektraEntryIncRefCount (key), // refs = 1
		elektraEntryBuild (ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo\0bar"), NULL, NULL),
		elektraEntryBuild (ELEKTRA_KEYNAME (ELEKTRA_NS_USER, "foo\0bar"), ELEKTRA_KEYVALUE_STRING ("hello"), NULL),
		elektraEntryBuild (ELEKTRA_KEYNAME (ELEKTRA_NS_PROC, "foo\0bar"), ELEKTRA_KEYVALUE_PTR (a),
				   ELEKTRA_KEYSET (elektraEntryBuildMeta (ELEKTRA_METANAME ("type"), ELEKTRA_KEYVALUE_STRING ("long")),
						   elektraEntryBuildMeta (ELEKTRA_METANAME ("check\0min"), ELEKTRA_KEYVALUE_STRING ("4")))),
		elektraEntryBuild (ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo\0bar\0abc"), NULL, NULL),
		elektraEntryBuild (ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo\0bar\0def"), ELEKTRA_KEYVALUE_STRING ("apple"), stringMeta),
		elektraEntryBuild (ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo\0bar\0ghi"), ELEKTRA_KEYVALUE_STRING ("banana"),
				   stringMeta)); // refs = 2, rest in ks has refs = 1

	// cleanup stringMeta
	// Note: could also be inlined on last use (but only on last use), without elektraSetDel (which would then happen inside
	// elektraEntryBuild)
	elektraSetDel ((ElektraSet *) elektraSetDecRefCount (stringMeta));

	elektraSetDel (ks); // refs = 1, rest of ks deleted

	// we can still use key now

	elektraEntryDecRefCount (key); // refs = 0
	elektraEntryDel (key);	       // key deleted
}

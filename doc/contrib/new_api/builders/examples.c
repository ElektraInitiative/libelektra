#include "public.h"

int main (void)
{
	int a = 7;

	ElektraEntry * key = ElektraEntryNew (&ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo")); // refs = 1

	ElektraSet * ks = ELEKTRA_KEYSET (
		// ElektraSetBuild consumes reference, doesn't retain itself
		// we call ElektraEntryRetain, so we can keep our own reference
		ElektraEntryRetain (key), // refs = 2
		ElektraEntryBuild (&ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo\0bar"), NULL, NULL),
		ElektraEntryBuild (&ELEKTRA_KEYNAME (ELEKTRA_NS_USER, "foo\0bar"), &ELEKTRA_KEYVALUE_STRING ("hello"), NULL),
		ElektraEntryBuild (
			&ELEKTRA_KEYNAME (ELEKTRA_NS_PROC, "foo\0bar"), &ELEKTRA_KEYVALUE_PTR (a),
			ELEKTRA_KEYSET (ElektraEntryBuildMeta (&ELEKTRA_METANAME ("type"), &ELEKTRA_KEYVALUE_STRING ("long")),
					ElektraEntryBuildMeta (&ELEKTRA_METANAME ("check\0min"), &ELEKTRA_KEYVALUE_STRING ("4")), )),
		ElektraEntryBuild (&ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo\0bar\0abc"), NULL, NULL)); // refs = 2, rest in ks has refs = 1

	ElektraSetRelease (ks); // refs = 1, rest of ks deleted

	ElektraEntryRelease (key); // refs = 0, key deleted
}

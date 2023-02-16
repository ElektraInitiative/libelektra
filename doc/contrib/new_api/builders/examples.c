#include "public.h"

int main (void)
{
	int a = 7;

	ElektraEntry * key = elektraEntryNew (&ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo")); // refs = 1

	ElektraSet * ks = ELEKTRA_KEYSET (
		// ElektraSetBuild consumes reference, doesn't retain itself
		// we call ElektraEntryRetain, so we can keep our own reference
		elektraEntryRetain (key), // refs = 2
		elektraEntryBuild (&ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo\0bar"), NULL, NULL),
		elektraEntryBuild (&ELEKTRA_KEYNAME (ELEKTRA_NS_USER, "foo\0bar"), &ELEKTRA_KEYVALUE_STRING ("hello"), NULL),
		elektraEntryBuild (
			&ELEKTRA_KEYNAME (ELEKTRA_NS_PROC, "foo\0bar"), &ELEKTRA_KEYVALUE_PTR (a),
			ELEKTRA_KEYSET (elektraEntryBuildMeta (&ELEKTRA_METANAME ("type"), &ELEKTRA_KEYVALUE_STRING ("long")),
					elektraEntryBuildMeta (&ELEKTRA_METANAME ("check\0min"), &ELEKTRA_KEYVALUE_STRING ("4")), )),
		elektraEntryBuild (&ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo\0bar\0abc"), NULL, NULL)); // refs = 2, rest in ks has refs = 1

	elektraSetRelease (ks); // refs = 1, rest of ks deleted

	elektraEntryRelease (key); // refs = 0, key deleted
}

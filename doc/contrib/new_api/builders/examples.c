#include "public.h"

int main (void)
{
	int a = 7;

	ElektraKey * key = elektraKeyNew (&ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo")); // refs = 1

	ElektraKeyset * ks = ELEKTRA_KEYSET (
		// elektraKeysetBuild consumes reference, doesn't retain itself
		// we call elektraKeyRetain, so we can keep our own reference
		elektraKeyRetain (key), // refs = 2
		elektraKeyBuild (&ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo\0bar"), NULL, NULL),
		elektraKeyBuild (&ELEKTRA_KEYNAME (ELEKTRA_NS_USER, "foo\0bar"), &ELEKTRA_KEYVALUE_STRING ("hello"), NULL),
		elektraKeyBuild (&ELEKTRA_KEYNAME (ELEKTRA_NS_PROC, "foo\0bar"), &ELEKTRA_KEYVALUE_PTR (a),
				 ELEKTRA_KEYSET (elektraKeyBuildMeta (&ELEKTRA_METANAME ("type"), &ELEKTRA_KEYVALUE_STRING ("long")),
						 elektraKeyBuildMeta (&ELEKTRA_METANAME ("check\0min"), &ELEKTRA_KEYVALUE_STRING ("4")), )),
		elektraKeyBuild (&ELEKTRA_KEYNAME (ELEKTRA_NS_SYSTEM, "foo\0bar\0abc"), NULL, NULL)); // refs = 2, rest in ks has refs = 1

	elektraKeysetRelease (ks); // refs = 1, rest of ks deleted

	elektraKeyRelease (key); // refs = 0, key deleted
}

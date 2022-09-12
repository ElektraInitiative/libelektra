/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <assert.h>
#include <kdb.h>
#include <stdio.h>

void f (const ElektraKey * source)
{
	ElektraKey * dup = elektraKeyDup (source, ELEKTRA_KEY_CP_ALL);
	printf ("\tin f\n");

	elektraKeyDel (dup);
}

void g (const ElektraKey * source, ElektraKeyset * ks)
{
	ElektraKey * dup = elektraKeyDup (source, ELEKTRA_KEY_CP_ALL);
	printf ("\tin g\n");

	elektraKeysetAppendKey (ks, dup);
}

void h (ElektraKey * k)
{
	ElektraKey * c = elektraKeyNew ("user:/from/h", ELEKTRA_KEY_END);
	printf ("\tin h\n");

	elektraKeyCopy (k, c, ELEKTRA_KEY_CP_ALL);
	elektraKeyDel (c);
	/* the caller will see the changed key k */
}

// clang-format off

void simpleAppend (void)
{
//! [simple append]
ElektraKeyset * ks = elektraKeysetNew (1, ELEKTRA_KS_END);
elektraKeysetAppendKey (ks, elektraKeyNew ("user:/my/new/key", ELEKTRA_KEY_END));
elektraKeysetDel (ks);
// key deleted, too!
//! [simple append]
}


void refAppend (void)
{
//! [ref append]
ElektraKeyset * ks = elektraKeysetNew (1, ELEKTRA_KS_END);
ElektraKey * k = elektraKeyNew ("user:/ref/key", ELEKTRA_KEY_END);
elektraKeyIncRef (k);
elektraKeysetAppendKey (ks, k);
elektraKeysetDel (ks);
// now we still can work with the key k!
elektraKeyDecRef (k);
elektraKeyDel (k);
//! [ref append]
}

void dupAppend (void)
{
//! [dup append]
ElektraKeyset * ks = elektraKeysetNew (1, ELEKTRA_KS_END);
ElektraKey * k = elektraKeyNew ("user:/ref/key", ELEKTRA_KEY_END);
elektraKeysetAppendKey (ks, elektraKeyDup (k, ELEKTRA_KEY_CP_ALL));
elektraKeysetDel (ks);
// now we still can work with the key k!
elektraKeyDel (k);
//! [dup append]
}

int main (void)
{
	ElektraKey * origKey;
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	ElektraKey * key = elektraKeyNew ("user:/test/name", ELEKTRA_KEY_VALUE, "myvalue", ELEKTRA_KEY_END);
	printf ("Created key %s with value %s\n", elektraKeyName (key), elektraKeyString (key));

	f (key);
	printf ("Key is unchanged with value %s\n", elektraKeyString (key));

	g (key, ks);
	printf ("A duplication was appended in keyset with name %s\n", elektraKeyName (elektraKeysetHead (ks)));

	h (key);
	printf ("Key has changed to name %s with value %s\n", elektraKeyName (key), elektraKeyString (key));

	simpleAppend ();
	refAppend ();
	dupAppend ();

	/* key is yet independent */
	elektraKeyDel (key);

	elektraKeysetRewind (ks);
	origKey = elektraKeysetNext (ks);
	key = elektraKeyDup (origKey, ELEKTRA_KEY_CP_ALL);
	printf ("A duplication of the key %s with value %s\n", elektraKeyName (key), elektraKeyString (key));

	elektraKeyDel (key);
	elektraKeysetDel (ks);
	return 0;
}

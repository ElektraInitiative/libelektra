/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <assert.h>
#include <elektra/kdb.h>
#include <stdio.h>

void f (const Key * source)
{
	Key * dup = keyDup (source, KEY_CP_ALL);
	printf ("\tin f\n");

	keyDel (dup);
}

void g (const Key * source, KeySet * ks)
{
	Key * dup = keyDup (source, KEY_CP_ALL);
	printf ("\tin g\n");

	ksAppendKey (ks, dup);
}

void h (Key * k)
{
	Key * c = keyNew ("user:/from/h", KEY_END);
	printf ("\tin h\n");

	keyCopy (k, c, KEY_CP_ALL);
	keyDel (c);
	/* the caller will see the changed key k */
}

// clang-format off

void simpleAppend (void)
{
//! [simple append]
KeySet * ks = ksNew (1, KS_END);
ksAppendKey (ks, keyNew ("user:/my/new/key", KEY_END));
ksDel (ks);
// key deleted, too!
//! [simple append]
}


void refAppend (void)
{
//! [ref append]
KeySet * ks = ksNew (1, KS_END);
Key * k = keyNew ("user:/ref/key", KEY_END);
keyIncRef (k);
ksAppendKey (ks, k);
ksDel (ks);
// now we still can work with the key k!
keyDecRef (k);
keyDel (k);
//! [ref append]
}

void dupAppend (void)
{
//! [dup append]
KeySet * ks = ksNew (1, KS_END);
Key * k = keyNew ("user:/ref/key", KEY_END);
ksAppendKey (ks, keyDup (k, KEY_CP_ALL));
ksDel (ks);
// now we still can work with the key k!
keyDel (k);
//! [dup append]
}

int main (void)
{
	Key * origKey;
	KeySet * ks = ksNew (0, KS_END);

	Key * key = keyNew ("user:/test/name", KEY_VALUE, "myvalue", KEY_END);
	printf ("Created key %s with value %s\n", keyName (key), keyString (key));

	f (key);
	printf ("Key is unchanged with value %s\n", keyString (key));

	g (key, ks);
	printf ("A duplication was appended in keyset with name %s\n", keyName (ksAtCursor(ks, 0)));

	h (key);
	printf ("Key has changed to name %s with value %s\n", keyName (key), keyString (key));

	simpleAppend ();
	refAppend ();
	dupAppend ();

	/* key is yet independent */
	keyDel (key);

	origKey = ksAtCursor(ks, 0);
	key = keyDup (origKey, KEY_CP_ALL);
	printf ("A duplication of the key %s with value %s\n", keyName (key), keyString (key));

	keyDel (key);
	ksDel (ks);
	return 0;
}

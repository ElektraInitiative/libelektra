/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

#include <kdb.h>
#include <stdio.h>

static void shortExamples(void)
{
{
//! [Simple]
ElektraKey *k = keyNew("/", ELEKTRA_KEY_END);
// work with it
keyDel (k);
//! [Simple]

}{

//! [Alternative]
ElektraKey *k =keyNew("", ELEKTRA_KEY_END); // Has the same effect as above
// work with it
keyDel (k);
//! [Alternative]

}{

//! [With Name]
// Create and initialize a key with a name and nothing else
ElektraKey *k=keyNew("user:/some/example", ELEKTRA_KEY_END);
// work with it
keyDel (k);
//! [With Name]

}{

//! [With Value]
// Create and initialize a key with a name and nothing else
ElektraKey *k=keyNew("user:/tmp/ex0",
	ELEKTRA_KEY_VALUE, "some data",    // set a string value
	ELEKTRA_KEY_END);                  // end of args
//! [With Value]
keyDel(k);

}{

//! [With Size]
// Create and initialize a key with a name and nothing else
ElektraKey *k=keyNew("user:/tmp/ex1",
	ELEKTRA_KEY_SIZE, 4,               // has no effect on strings
	ELEKTRA_KEY_VALUE, "some data",    // set a string value
	ELEKTRA_KEY_END);                  // end of args
//! [With Size]
printf ("%s\n", keyString(k));
keyDel(k);

}{

//! [With Binary]
// Create and initialize a key with a name and nothing else
ElektraKey *k=keyNew("user:/tmp/ex2",
	ELEKTRA_KEY_BINARY,
	ELEKTRA_KEY_SIZE, 4,               // now the size is important
	ELEKTRA_KEY_VALUE, "some data",    // sets the binary value ("some")
	ELEKTRA_KEY_END);                  // end of args
//! [With Binary]
printf ("%.4s\n", (char*)keyValue(k));
keyDel(k);


}{

//! [With Meta]
ElektraKey *k=keyNew("user:/tmp/ex3",
	ELEKTRA_KEY_META, "comment", "a comment",  // with a comment
	ELEKTRA_KEY_META, "owner", "root",         // and an owner
	ELEKTRA_KEY_META, "special", "yes",        // and any other metadata
	ELEKTRA_KEY_END);                  // end of args
//! [With Meta]
keyDel(k);

}{

//! [With Flags]
ElektraKey *k=keyNew("user:/tmp/ex3",
	ELEKTRA_KEY_BINARY,			// binary key
	ELEKTRA_KEY_SIZE, 7,			// assume binary length 7
	ELEKTRA_KEY_VALUE, "some data",		// value that will be truncated in 7 bytes
	ELEKTRA_KEY_END);			// end of args
//! [With Flags]
printf ("%.7s\n", (char*)keyValue(k));
keyDel(k);

}{

//! [With Everything]
ElektraKey *k=keyNew("user:/tmp/ex4",
	ELEKTRA_KEY_BINARY,			// key type
	ELEKTRA_KEY_SIZE, 7,			// assume binary length 7
	ELEKTRA_KEY_VALUE, "some data",		// value that will be truncated in 7 bytes
	ELEKTRA_KEY_COMMENT, "value is truncated",
	ELEKTRA_KEY_END);			// end of args
//! [With Everything]
printf ("%.7s\n", (char*)keyValue(k));
keyDel(k);

}{

//! [Ref in KeySet]
ElektraKey *k = keyNew("user:/proper_name", ELEKTRA_KEY_END); // ref counter = 0
ElektraKeyset *ks = ksNew (1, k, ELEKTRA_KS_END);
keyDel(k); // key will not be deleted, because its in the keyset
ksDel(ks); // now the key will be deleted
//! [Ref in KeySet]

}{

//! [Ref in multiple KeySets]
ElektraKey *k = keyNew("user:/proper_name", ELEKTRA_KEY_END); // ref counter 0
ElektraKeyset *ks1 = ksNew(1, k, ELEKTRA_KS_END); // ref counter of k 1
ElektraKeyset *ks2 = ksNew(1, k, ELEKTRA_KS_END); // ref counter of k 2
ksDel(ks1); // ref counter of k 1
ksDel(ks2); // k is now deleted
//! [Ref in multiple KeySets]

}{

//! [Ref]
ElektraKey *k = keyNew("/", ELEKTRA_KEY_END); // ref counter = 0
keyIncRef(k); // ref counter = 1
keyDel(k); // key will not be deleted
keyDecRef(k);
keyDel(k);
//! [Ref]

}{

//! [Multi Ref]
ElektraKey *k = keyNew("/", ELEKTRA_KEY_END); // ref counter 0
keyIncRef(k); // ref counter of key 1
keyDel (k);   // has no effect
keyIncRef(k); // ref counter of key 2
keyDel (k);   // has no effect
keyDecRef(k); // ref counter of key 1
keyDel (k);   // has no effect
keyDecRef(k); // ref counter is now 0
keyDel (k); // k is now deleted
//! [Multi Ref]

}

}

int main(void)
{
	shortExamples();
}

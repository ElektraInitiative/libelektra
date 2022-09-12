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
ElektraKey *k = elektraKeyNew("/", ELEKTRA_KEY_END);
// work with it
elektraKeyDel (k);
//! [Simple]

}{

//! [Alternative]
ElektraKey *k =elektraKeyNew("", ELEKTRA_KEY_END); // Has the same effect as above
// work with it
elektraKeyDel (k);
//! [Alternative]

}{

//! [With Name]
// Create and initialize a key with a name and nothing else
ElektraKey *k=elektraKeyNew("user:/some/example", ELEKTRA_KEY_END);
// work with it
elektraKeyDel (k);
//! [With Name]

}{

//! [With Value]
// Create and initialize a key with a name and nothing else
ElektraKey *k=elektraKeyNew("user:/tmp/ex0",
	ELEKTRA_KEY_VALUE, "some data",    // set a string value
	ELEKTRA_KEY_END);                  // end of args
//! [With Value]
elektraKeyDel(k);

}{

//! [With Size]
// Create and initialize a key with a name and nothing else
ElektraKey *k=elektraKeyNew("user:/tmp/ex1",
	ELEKTRA_KEY_SIZE, 4,               // has no effect on strings
	ELEKTRA_KEY_VALUE, "some data",    // set a string value
	ELEKTRA_KEY_END);                  // end of args
//! [With Size]
printf ("%s\n", elektraKeyString(k));
elektraKeyDel(k);

}{

//! [With Binary]
// Create and initialize a key with a name and nothing else
ElektraKey *k=elektraKeyNew("user:/tmp/ex2",
	ELEKTRA_KEY_BINARY,
	ELEKTRA_KEY_SIZE, 4,               // now the size is important
	ELEKTRA_KEY_VALUE, "some data",    // sets the binary value ("some")
	ELEKTRA_KEY_END);                  // end of args
//! [With Binary]
printf ("%.4s\n", (char*)elektraKeyValue(k));
elektraKeyDel(k);


}{

//! [With Meta]
ElektraKey *k=elektraKeyNew("user:/tmp/ex3",
	ELEKTRA_KEY_META, "comment", "a comment",  // with a comment
	ELEKTRA_KEY_META, "owner", "root",         // and an owner
	ELEKTRA_KEY_META, "special", "yes",        // and any other metadata
	ELEKTRA_KEY_END);                  // end of args
//! [With Meta]
elektraKeyDel(k);

}{

//! [With Flags]
ElektraKey *k=elektraKeyNew("user:/tmp/ex3",
	ELEKTRA_KEY_BINARY,			// binary key
	ELEKTRA_KEY_SIZE, 7,			// assume binary length 7
	ELEKTRA_KEY_VALUE, "some data",		// value that will be truncated in 7 bytes
	ELEKTRA_KEY_END);			// end of args
//! [With Flags]
printf ("%.7s\n", (char*)elektraKeyValue(k));
elektraKeyDel(k);

}{

//! [With Everything]
ElektraKey *k=elektraKeyNew("user:/tmp/ex4",
	ELEKTRA_KEY_BINARY,			// key type
	ELEKTRA_KEY_SIZE, 7,			// assume binary length 7
	ELEKTRA_KEY_VALUE, "some data",		// value that will be truncated in 7 bytes
	ELEKTRA_KEY_COMMENT, "value is truncated",
	ELEKTRA_KEY_END);			// end of args
//! [With Everything]
printf ("%.7s\n", (char*)elektraKeyValue(k));
elektraKeyDel(k);

}{

//! [Ref in KeySet]
ElektraKey *k = elektraKeyNew("user:/proper_name", ELEKTRA_KEY_END); // ref counter = 0
ElektraKeyset *ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);
elektraKeyDel(k); // key will not be deleted, because its in the keyset
elektraKeysetDel(ks); // now the key will be deleted
//! [Ref in KeySet]

}{

//! [Ref in multiple KeySets]
ElektraKey *k = elektraKeyNew("user:/proper_name", ELEKTRA_KEY_END); // ref counter 0
ElektraKeyset *ks1 = elektraKeysetNew(1, k, ELEKTRA_KS_END); // ref counter of k 1
ElektraKeyset *ks2 = elektraKeysetNew(1, k, ELEKTRA_KS_END); // ref counter of k 2
elektraKeysetDel(ks1); // ref counter of k 1
elektraKeysetDel(ks2); // k is now deleted
//! [Ref in multiple KeySets]

}{

//! [Ref]
ElektraKey *k = elektraKeyNew("/", ELEKTRA_KEY_END); // ref counter = 0
elektraKeyIncRef(k); // ref counter = 1
elektraKeyDel(k); // key will not be deleted
elektraKeyDecRef(k);
elektraKeyDel(k);
//! [Ref]

}{

//! [Multi Ref]
ElektraKey *k = elektraKeyNew("/", ELEKTRA_KEY_END); // ref counter 0
elektraKeyIncRef(k); // ref counter of key 1
elektraKeyDel (k);   // has no effect
elektraKeyIncRef(k); // ref counter of key 2
elektraKeyDel (k);   // has no effect
elektraKeyDecRef(k); // ref counter of key 1
elektraKeyDel (k);   // has no effect
elektraKeyDecRef(k); // ref counter is now 0
elektraKeyDel (k); // k is now deleted
//! [Multi Ref]

}

}

int main(void)
{
	shortExamples();
}

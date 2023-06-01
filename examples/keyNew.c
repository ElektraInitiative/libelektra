/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <stdio.h>

static void shortExamples(void)
{
{
//! [Simple]
Key *k = keyNew("/", KEY_END);
// work with it
keyDel (k);
//! [Simple]

}{

//! [Alternative]
Key *k =keyNew("", KEY_END); // Has the same effect as above
// work with it
keyDel (k);
//! [Alternative]

}{

//! [With Name]
// Create and initialize a key with a name and nothing else
Key *k=keyNew("user:/some/example", KEY_END);
// work with it
keyDel (k);
//! [With Name]

}{

//! [With Value]
// Create and initialize a key with a name and nothing else
Key *k=keyNew("user:/tmp/ex0",
	KEY_VALUE, "some data",    // set a string value
	KEY_END);                  // end of args
//! [With Value]
keyDel(k);

}{

//! [With Size]
// Create and initialize a key with a name and nothing else
Key *k=keyNew("user:/tmp/ex1",
	KEY_SIZE, 4,               // has no effect on strings
	KEY_VALUE, "some data",    // set a string value
	KEY_END);                  // end of args
//! [With Size]
printf ("%s\n", keyString(k));
keyDel(k);

}{

//! [With Binary]
// Create and initialize a key with a name and nothing else
Key *k=keyNew("user:/tmp/ex2",
	KEY_BINARY,
	KEY_SIZE, 4,               // now the size is important
	KEY_VALUE, "some data",    // sets the binary value ("some")
	KEY_END);                  // end of args
//! [With Binary]
printf ("%.4s\n", (char*)keyValue(k));
keyDel(k);


}{

//! [With Meta]
Key *k=keyNew("user:/tmp/ex3",
	KEY_META, "comment/#0", "a comment",  // with a comment
	KEY_META, "owner", "root",         // and an owner
	KEY_META, "special", "yes",        // and any other metadata
	KEY_END);                  // end of args
//! [With Meta]
keyDel(k);

}{

//! [With Flags]
Key *k=keyNew("user:/tmp/ex3",
	KEY_BINARY,			// binary key
	KEY_SIZE, 7,			// assume binary length 7
	KEY_VALUE, "some data",		// value that will be truncated in 7 bytes
	KEY_END);			// end of args
//! [With Flags]
printf ("%.7s\n", (char*)keyValue(k));
keyDel(k);

}{

//! [With Everything]
Key *k=keyNew("user:/tmp/ex4",
	KEY_BINARY,			// key type
	KEY_SIZE, 7,			// assume binary length 7
	KEY_VALUE, "some data",		// value that will be truncated in 7 bytes
	KEY_META, "comment/#0", "value is truncated",
	KEY_END);			// end of args
//! [With Everything]
printf ("%.7s\n", (char*)keyValue(k));
keyDel(k);

}{

//! [Ref in KeySet]
Key *k = keyNew("user:/proper_name", KEY_END); // ref counter = 0
KeySet *ks = ksNew (1, k, KS_END);
keyDel(k); // key will not be deleted, because its in the keyset
ksDel(ks); // now the key will be deleted
//! [Ref in KeySet]

}{

//! [Ref in multiple KeySets]
Key *k = keyNew("user:/proper_name", KEY_END); // ref counter 0
KeySet *ks1 = ksNew(1, k, KS_END); // ref counter of k 1
KeySet *ks2 = ksNew(1, k, KS_END); // ref counter of k 2
ksDel(ks1); // ref counter of k 1
ksDel(ks2); // k is now deleted
//! [Ref in multiple KeySets]

}{

//! [Ref]
Key *k = keyNew("/", KEY_END); // ref counter = 0
keyIncRef(k); // ref counter = 1
keyDel(k); // key will not be deleted
keyDecRef(k);
keyDel(k);
//! [Ref]

}{

//! [Multi Ref]
Key *k = keyNew("/", KEY_END); // ref counter 0
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

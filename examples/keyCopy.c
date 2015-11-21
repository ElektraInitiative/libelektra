/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <kdb.h>
#include <stdlib.h>
#include <string.h>

Key *c;

//! [Basic Usage]
void h (Key *k)
{
	// receive key c
	keyCopy (k, c);
	// the caller will see the changed key k
}
//! [Basic Usage]

//! [Clear]
void g (Key *k)
{
	keyCopy (k, 0);
	// k is now an empty and fresh key
}
//! [Clear]

//! [Copy Without Value]
void j (Key *k)
{
	size_t size = keyGetValueSize (k);
	char *value = elektraMalloc (size);
	int bstring = keyIsString (k);

	// receive key c
	memcpy (value, keyValue(k), size);
	keyCopy (k, c);
	if (bstring) keySetString (k, value);
	else keySetBinary (k, value, size);
	free (value);
	// the caller will see the changed key k
	// with the name and metadata from c (except
	// metadata "binary", which stayed the same)
}
//! [Copy Without Value]

//! [Individual Copy]
void i (Key *k)
{
	keySetName(k, keyName(c));
	keySetString(k, keyString(c));
	keyCopyAllMeta(k, c);
	// k is not a copy of c even if everything was successfully,
	// because it still contains meta data from k
}
//! [Individual Copy]

int main()
{
	Key * k = keyNew("user/hello",
		KEY_VALUE, "my content",
		KEY_END);

	c = keyNew("user/copy",
		KEY_VALUE, "copies content",
		KEY_END);

	h(k);
	g(k);
	j(k);
	i(k);

	keyDel(k);
	keyDel(c);
}

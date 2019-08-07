/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>
#include <stdlib.h>
#include <string.h>

Key * copy;

//! [Basic Usage]
void h (Key * k)
{
	// receive key c
	keyCopy (k, copy);
	// the caller will see the changed key k
}
//! [Basic Usage]

//! [Clear]
void g (Key * k)
{
	keyCopy (k, 0);
	// k is now an empty and fresh key
}
//! [Clear]

//! [Copy Without Value]
void j (Key * k)
{
	size_t size = keyGetValueSize (k);
	char * value = malloc (size);
	int bstring = keyIsString (k);

	// receive key c
	memcpy (value, keyValue (k), size);
	keyCopy (k, copy);
	if (bstring)
		keySetString (k, value);
	else
		keySetBinary (k, value, size);
	free (value);
	// the caller will see the changed key k
	// with the name and metadata from copy (except
	// metadata "binary", which stayed the same)
}
//! [Copy Without Value]

//! [Individual Copy]
void i (Key * k)
{
	keySetName (k, keyName (copy));
	keySetString (k, keyString (copy));
	keyCopyAllMeta (k, copy);
	// k is not a copy of copy even if everything was successfully,
	// because it still contains metadata from k
}
//! [Individual Copy]

int main (void)
{
	Key * k = keyNew ("user/hello", KEY_VALUE, "my content", KEY_END);

	copy = keyNew ("user/copy", KEY_VALUE, "copies content", KEY_END);

	h (k);
	g (k);
	j (k);
	i (k);

	keyDel (k);
	keyDel (copy);
}

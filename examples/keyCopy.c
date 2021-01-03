/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stdlib.h>
#include <string.h>

Key * copy;

//! [Duplicate Key]
void x (Key * orig)
{
	copy = keyCopy (keyNew ("/", KEY_END), orig, ~0);
}
//! [Duplicate Key]

//! [Basic Usage]
void h (Key * orig)
{
	keyCopy (copy, orig, ~0);
}
//! [Basic Usage]

//! [Clear]
void g (Key * k)
{
	keyCopyOld (k, 0);
	// k is now an empty and fresh key
}
//! [Clear]

int main (void)
{
	Key * k = keyNew ("user:/hello", KEY_VALUE, "my content", KEY_END);

	copy = keyNew ("user:/copy", KEY_VALUE, "copies content", KEY_END);

	x (k);
	h (k);
	g (k);

	keyDel (k);
	keyDel (copy);
}

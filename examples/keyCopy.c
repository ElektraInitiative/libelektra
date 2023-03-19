/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/old_kdb.h>
#include <stdlib.h>
#include <string.h>

Key * copy;

// clang-format off
void x (Key * orig)
{
//! [Duplicate Key]
copy = keyCopy (keyNew ("/", KEY_END), orig, KEY_CP_ALL);
//! [Duplicate Key]
}

void y (Key * orig)
{
//! [Dup Key]
copy = keyDup (orig, KEY_CP_ALL);
//! [Dup Key]
}

void h (Key * orig)
{
//! [Basic Usage]
keyCopy (copy, orig, KEY_CP_ALL);
//! [Basic Usage]
}

void g (Key * k)
{
//! [Clear]
keyCopy (k, NULL, KEY_CP_ALL);
// name, value and metadata of k have now been clear
// lock flags, reference count, etc. remain unchanged
//! [Clear]
}

// clang-format on

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

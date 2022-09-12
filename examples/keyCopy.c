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

ElektraKey * copy;

// clang-format off
void x (ElektraKey * orig)
{
//! [Duplicate Key]
copy = keyCopy (keyNew ("/", ELEKTRA_KEY_END), orig, ELEKTRA_KEY_CP_ALL);
//! [Duplicate Key]
}

void y (ElektraKey * orig)
{
//! [Dup Key]
copy = keyDup (orig, ELEKTRA_KEY_CP_ALL);
//! [Dup Key]
}

void h (ElektraKey * orig)
{
//! [Basic Usage]
keyCopy (copy, orig, ELEKTRA_KEY_CP_ALL);
//! [Basic Usage]
}

void g (ElektraKey * k)
{
//! [Clear]
keyCopy (k, NULL, ELEKTRA_KEY_CP_ALL);
// name, value and metadata of k have now been clear
// lock flags, reference count, etc. remain unchanged
//! [Clear]
}

// clang-format on

int main (void)
{
	ElektraKey * k = keyNew ("user:/hello", ELEKTRA_KEY_VALUE, "my content", ELEKTRA_KEY_END);

	copy = keyNew ("user:/copy", ELEKTRA_KEY_VALUE, "copies content", ELEKTRA_KEY_END);

	x (k);
	h (k);
	g (k);

	keyDel (k);
	keyDel (copy);
}

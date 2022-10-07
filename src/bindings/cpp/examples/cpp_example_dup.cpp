/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <keyset.hpp>

//! [ksDeepCopy]
kdb::KeySet ksDeepCopy (kdb::KeySet orig)
{
	kdb::KeySet deepCopy;

	for (ssize_t it = 0; it < orig.size (); ++it)
	{
		deepCopy.append (orig.at (it).dup ());
	}
	return deepCopy;
}
//! [ksDeepCopy]

int main ()
{
	kdb::KeySet orig (3, *kdb::Key ("user:/key3/1", KEY_END), *kdb::Key ("user:/key3/2", KEY_END),
			  *kdb::Key ("user:/key3/3", KEY_VALUE, "value", KEY_END), KS_END);
	kdb::KeySet flatCopy (orig);
	kdb::KeySet deepCopy = ksDeepCopy (orig);
}

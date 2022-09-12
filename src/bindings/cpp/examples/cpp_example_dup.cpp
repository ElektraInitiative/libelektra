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
	orig.rewind ();
	while (orig.next ())
	{
		deepCopy.append (orig.current ().dup ());
	}
	return deepCopy;
}
//! [ksDeepCopy]

int main ()
{
	kdb::KeySet orig (3, *kdb::Key ("user:/key3/1", ELEKTRA_KEY_END), *kdb::Key ("user:/key3/2", ELEKTRA_KEY_END),
			  *kdb::Key ("user:/key3/3", ELEKTRA_KEY_VALUE, "value", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	kdb::KeySet flatCopy (orig);
	kdb::KeySet deepCopy = ksDeepCopy (orig);
}

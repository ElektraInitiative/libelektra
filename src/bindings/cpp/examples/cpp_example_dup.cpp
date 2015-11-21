/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <keyset.hpp>

//! [ksDeepCopy]
kdb::KeySet ksDeepCopy(kdb::KeySet orig)
{
	kdb::KeySet deepCopy;
	orig.rewind();
	while(orig.next())
	{
		deepCopy.append(orig.current().dup());
	}
	return deepCopy;
}
//! [ksDeepCopy]

int main()
{
	kdb::KeySet orig(3,
			*kdb::Key ("user/key3/1", KEY_END),
			*kdb::Key ("user/key3/2", KEY_END),
			*kdb::Key ("user/key3/3",
				KEY_VALUE, "value", KEY_END),
		KS_END);
	kdb::KeySet flatCopy(orig);
	kdb::KeySet deepCopy = ksDeepCopy(orig);

}

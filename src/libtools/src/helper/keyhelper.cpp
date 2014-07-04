/**
 * \file
 *
 * \brief Key helper functions
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#include <helper/keyhelper.hpp>

using namespace std;

namespace kdb
{

namespace tools
{

namespace helper
{

string rebasePath(const Key& key, const Key& oldParent,
		const Key& newParent)
{
	string oldKeyPath = key.getName ();

	if (!oldParent.isBelowOrSame(key)) throw InvalidRebaseException("the supplied key is not below the old parent");

	string relativePath = oldKeyPath.substr (oldParent.getName().length (),
			oldKeyPath.length ());
	string newPath = newParent.getName () + relativePath;

	return newPath;
}

Key rebaseKey(const Key& key, const Key& oldParent,
		const Key& newParent)
{
	string newPath = rebasePath (key, oldParent, newParent);
	Key result = key.dup ();
	result.setName (newPath);
	return result;
}

}
}
}

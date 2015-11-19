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

	Key actualOldParent = oldParent.dup();
	if (oldParent.getNamespace() == "/")
	{
		actualOldParent.setName(key.getNamespace() + oldParent.getName());
	}

	Key actualNewParent = newParent.dup();
	if (newParent.getNamespace() == "/")
	{
		actualNewParent.setName(key.getNamespace() + newParent.getName());
	}

	if (!key.isBelowOrSame(actualOldParent)) throw InvalidRebaseException(
			"the supplied key " +
			key.getName() +
			" is not below the old parent " +
			actualOldParent.getName());

	string relativePath = oldKeyPath.substr (actualOldParent.getName().length (),
			oldKeyPath.length ());
	string newPath = actualNewParent.getName () + relativePath;

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

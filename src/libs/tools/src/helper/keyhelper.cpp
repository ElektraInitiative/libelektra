/**
 * @file
 *
 * @brief Key helper functions
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
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

string rebasePath (const Key & key, const Key & oldParent, const Key & newParent)
{
	string oldKeyPath = key.getName ();

	Key actualOldParent = oldParent.dup ();
	if (oldParent.getNamespace () == "/")
	{
		actualOldParent.setName (key.getNamespace () + oldParent.getName ());
	}

	Key actualNewParent = newParent.dup ();
	if (newParent.getNamespace () == "/")
	{
		actualNewParent.setName (key.getNamespace () + newParent.getName ());
	}

	if (!key.isBelowOrSame (actualOldParent))
		throw InvalidRebaseException ("the supplied key " + key.getName () + " is not below the old parent " +
					      actualOldParent.getName ());

	string relativePath = oldKeyPath.substr (actualOldParent.getName ().length (), oldKeyPath.length ());
	string newPath = actualNewParent.getName () + relativePath;

	return newPath;
}

Key rebaseKey (const Key & key, const Key & oldParent, const Key & newParent)
{
	string newPath = rebasePath (key, oldParent, newParent);
	Key result = key.dup ();
	result.setName (newPath);
	return result;
}

void removeNamespace (Key & key)
{
	std::string name = key.getName ();
	size_t pos = name.find_first_of ('/');
	if (pos == string::npos)
	{
		// we directly had a namespace
		key.setName ("/");
	}
	else
	{
		name = name.substr (pos);
		key.setName (name);
	}
}

Key commonKeyName (Key key1, Key key2)
{
	// do not let removed namespaces escape
	key1 = key1.dup ();
	key2 = key2.dup ();

	if (key1.isBelowOrSame (key2))
		return key2;
	if (key2.isBelowOrSame (key1))
		return key1;

	if (key1.getNamespace () != key2.getNamespace ())
	{
		removeNamespace (key1);
		removeNamespace (key2);
	}

	Key ret (key1.getNamespace (), KEY_END);
	for (auto it1 = ++key1.begin (), it2 = ++key2.begin (); it1 != key1.end () && it2 != key2.end (); ++it1, ++it2)
	{
		if (*it1 != *it2)
			break;
		ret.addBaseName (*it1);
	}
	return ret;
}
}
}
}

/**
 * @file
 *
 * @brief Key helper functions
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <helper/keyhelper.hpp>
#include <elektra/kdbprivate.h>

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
	if (oldParent.getNamespace () == ElektraNamespace::CASCADING)
	{
		actualOldParent.setName (oldParent.getName ());
		actualOldParent.setNamespace (key.getNamespace ());
	}

	Key actualNewParent = newParent.dup ();
	if (newParent.getNamespace () == ElektraNamespace::CASCADING)
	{
		actualNewParent.setName (newParent.getName ());
		actualNewParent.setNamespace (key.getNamespace ());
	}

	if (!key.isBelowOrSame (actualOldParent))
		throw InvalidRebaseException ("the supplied key " + key.getName () + " is not below the old parent " +
					      actualOldParent.getName ());

	string relativePath;
	if (oldKeyPath[0] == '/')
	{
		string actualOldParentName = actualOldParent.getName ();
		string withoutNamespaceParent = actualOldParentName.substr (actualOldParentName.find ('/'));
		relativePath = oldKeyPath.substr (withoutNamespaceParent.length ());
	}
	else
	{
		relativePath = oldKeyPath.substr (actualOldParent.getName ().length ());
	}

	if (relativePath.length () > 0 && relativePath[0] != '/')
	{
		relativePath = "/" + relativePath;
	}


	string newPath = actualNewParent.getName () + relativePath;

	return newPath;
}

Key rebaseKey (const Key & key, const Key & oldParent, const Key & newParent)
{
	Key result = key.dup ();
	ckdb::keyReplacePrefix (*result, *oldParent, *newParent);
	return result;
}

void removeNamespace (Key & key)
{
	std::string name = key.getName ();
	size_t pos = name.find_first_of (':');
	if (pos != string::npos)
	{
		name = name.substr (pos + 1);
		key.setName (name);
	}
}

Key prependNamespace (Key const & root, std::string const & ns)
{
	Key ret = root.dup ();
	if (ret.isCascading ())
	{
		ret.setName (ns + root.getName ());
	}
	return ret;
}

KeySet prependNamespace (KeySet const & resultKeys, std::string const & ns)
{
	KeySet ret;
	for (auto const & k : resultKeys)
	{
		ret.append (prependNamespace (k, ns));
	}
	return ret;
}

void copyAllMeta (KeySet & to, KeySet const & from)
{
	for (auto k : to)
	{
		Key b = from.lookup (k, 0);
		if (b)
		{
			k.copyAllMeta (b);
		}
	}
}

Key commonKeyName (Key key1, Key key2)
{
	// do not let removed namespaces escape
	key1 = key1.dup ();
	key2 = key2.dup ();

	if (key1.isBelowOrSame (key2)) return key2;
	if (key2.isBelowOrSame (key1)) return key1;

	if (key1.getNamespace () != key2.getNamespace ())
	{
		removeNamespace (key1);
		removeNamespace (key2);
	}

	auto ns = key1.getNamespace ();

	Key ret ("/", KEY_END);
	ret.setNamespace (ns);
	for (auto it1 = ++key1.begin (), it2 = ++key2.begin (); it1 != key1.end () && it2 != key2.end (); ++it1, ++it2)
	{
		if (*it1 != *it2) break;
		ret.addBaseName (*it1);
	}
	return ret;
}
} // namespace helper
} // namespace tools
} // namespace kdb

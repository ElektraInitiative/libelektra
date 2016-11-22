/**
 * @file
 *
 * @brief Key helper functions
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 *
 */

#ifndef KEYHELPER_HPP_
#define KEYHELPER_HPP_

#include <kdb.hpp>
#include <string>
#include <toolexcept.hpp>

namespace kdb
{

namespace tools
{

namespace helper
{

class InvalidRebaseException : public ToolException
{
public:
	InvalidRebaseException (std::string message) : ToolException (message){};
};

/**
 * Rebases the supplied key from the old parent to the new parent.
 *
 * @see ThreeWayMerge::rebasePath
 * @param key the key to be rebased
 * @param oldParent the old parent of the key
 * @param newParent the new parent of the key
 * @return a rebased copy of the supplied key
 * @throws InvalidRebaseException if the key is not below the old parent
 */
Key rebaseKey (const Key & key, const Key & oldParent, const Key & newParent);

/**
 * Rebases the relative path of the passed key from the old parent
 * to the new parent and returns the new path.
 *
 * For example a key /user/example/config/key1 with the oldparent
 * /user/example and the new parent /user/newexample/newpath would
 * result in /user/newexample/newpath/config/key1
 *
 * If any of the parent keys is a cascading key the namespace of the
 * key to be rebased is assumed instead.
 *
 * @param key the key whose path should be rebased
 * @param oldParent the old parent of the key
 * @param newParent the new parent of the key
 * @return the rebased path
 * @throws InvalidRebaseException if the key is not below the old parent
 */
std::string rebasePath (const Key & key, const Key & oldParent, const Key & newParent);

/**
 * @brief Removes the namespace
 *
 * @param key will be made to a cascading key
 */
void removeNamespace (Key & key);

/**
 * @brief Find common name between two keys
 *
 * @return the longest common name found
 */
Key commonKeyName (Key key1, Key key2);
}
}
}

#endif /* KEYHELPER_HPP_ */

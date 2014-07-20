/**
 * \file
 *
 * \brief Key helper functions
 *
 * \copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef KEYHELPER_HPP_
#define KEYHELPER_HPP_

#include <string>
#include <stdexcept>
#include <kdb.hpp>

namespace kdb
{

namespace tools
{

namespace helper
{

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
Key rebaseKey(const Key& key, const Key& oldParent, const Key& newParent);

/**
 * Rebases the relative path of the passed key from the old parent
 * to the new parent and returns the new path.
 *
 * For example a key /user/example/config/key1 with the oldparent
 * /user/example and the new parent /user/newexample/newpath would
 * result in /user/newexample/newpath/config/key1
 *
 * @param key the key whose path should be rebased
 * @param oldParent the old parent of the key
 * @param newParent the new parent of the key
 * @return the rebased path
 * @throws InvalidRebaseException if the key is not below the old parent
 */
std::string rebasePath(const Key& key, const Key& oldParent,
		const Key& newParent);

class InvalidRebaseException: public std::runtime_error
{
public:
	InvalidRebaseException(std::string _message) :
			runtime_error (_message)
	{
	}
	;
};
}
}
}

#endif /* KEYHELPER_HPP_ */

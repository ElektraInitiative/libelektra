/**
 * @file
 *
 * @brief Delegate definitions for the `leaf` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CPP_LEAF_DELEGATE_HPP
#define ELEKTRA_CPP_LEAF_DELEGATE_HPP

#include <kdberrors.h>
#include <kdbplugin.hpp>

namespace elektra
{
using std::pair;

using CppKeySet = kdb::KeySet;

class LeafDelegate
{
	/**
	 * @brief Split `keys` into two key sets, one for directories (keys without children) and one for all other keys.
	 *
	 * @param keys This parameter contains the key set this function splits.
	 *
	 * @return A pair of key sets, where the firs key set contains all directories and the second key set contains all leaves
	 */
	pair<CppKeySet, CppKeySet> splitDirectoriesLeaves (CppKeySet const & keys);

	/**
	 * @brief Convert all keys in `directories` to an empty key and a leaf key containing the data of the old key.
	 *
	 * @param directories This parameter contains a set of directory keys this function converts.
	 *
	 * @return A key set containing only empty directory keys and corresponding leaf keys storing the values of the old directory keys
	 */
	CppKeySet convertDirectoriesToLeaves (CppKeySet const & directories);

public:
	/**
	 * @brief This constructor creates a new delegate object used by the `leaf` plugin
	 *
	 * @param config This key set contains configuration values provided by the `leaf` plugin
	 */
	explicit LeafDelegate (CppKeySet config);

	/**
	 * @brief This method converts all directories keys in the given key set to leaf keys.
	 */
	void convertToLeaves (CppKeySet & keys);
};

} // end namespace elektra

#endif

/**
 * @file
 *
 * @brief Delegate implementation for the `leaf` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "leaf_delegate.hpp"

using std::make_pair;
using std::pair;

namespace elektra
{
using CppKey = kdb::Key;

// ===========
// = Private =
// ===========

/**
 * @brief Split `keys` into two key sets, one for directories (keys without children) and one for all other keys.
 *
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the firs key set contains all directories and the second key set contains all leaves
 */
pair<CppKeySet, CppKeySet> LeafDelegate::splitDirectoriesLeaves (CppKeySet const & keys)
{
	CppKeySet leaves;
	CppKeySet directories;

	keys.rewind ();
	for (CppKey previous = nullptr; keys.next (); previous = keys.current ())
	{
		if (keys.current ()->isBelow (previous))
		{
			leaves.append (keys.current ());
		}
		else
		{
			directories.append (keys.current ());
		}
	}
	return make_pair (directories, leaves);
}

// ==========
// = Public =
// ==========

/**
 * @brief This constructor creates a new delegate object used by the `leaf` plugin
 *
 * @param config This key set contains configuration values provided by the `leaf` plugin
 */
LeafDelegate::LeafDelegate (CppKeySet config ELEKTRA_UNUSED)
{
}

/**
 * @brief This method converts all directories keys in the given key set to leaf keys.
 */
void LeafDelegate::convertToLeaves (CppKeySet & keys)
{
	auto directoriesLeaves = splitDirectoriesLeaves (keys);
	keys.clear ();
	keys.append (directoriesLeaves.first);
	keys.append (directoriesLeaves.second);
}

} // end namespace elektra

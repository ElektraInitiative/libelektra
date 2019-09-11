/**
 * @file
 *
 * @brief Delegate definitions for the `directoryvalue` plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CPP_DIRECTORY_VALUE_DELEGATE_HPP
#define ELEKTRA_CPP_DIRECTORY_VALUE_DELEGATE_HPP

#include <kdberrors.h>
#include <kdbplugin.hpp>

#define DIRECTORY_POSTFIX "___dirdata"
#define ARRAY_VALUE_PREFIX "___dirdata:"

namespace elektra
{
using std::pair;
using std::string;

using CppKeySet = kdb::KeySet;

constexpr ssize_t arrayValuePrefixSize = sizeof (ARRAY_VALUE_PREFIX) - 1;

typedef pair<CppKeySet, CppKeySet> KeySetPair;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

/**
 * @brief Split `keys` into two key sets, one for array parents and one for all other keys.
 *
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the first key set contains all array parents and the second key set contains all other keys
 */
KeySetPair splitArrayParentsOther (CppKeySet const & keys);

/**
 * @brief Increase the array index of array elements by one.
 *
 * Since it is also possible that one of the array parents is part of another array, this function also updates the indices of the given
 * array parents.
 *
 * @param parents This parameter contains the array parents for which this function increases the index by one.
 * @param parents This variable stores the arrays elements this function updates.
 *
 * @return A pair containing a copy of `parents` and `arrays`, where all indices specified by `parents` are increased by one
 */
KeySetPair increaseArrayIndices (CppKeySet const & parents, CppKeySet const & arrays);

// -- Class --------------------------------------------------------------------------------------------------------------------------------

class DirectoryValueDelegate
{
public:
	/**
	 * @brief This constructor creates a new delegate object used by the `directoryvalue` plugin
	 *
	 * @param config This key set contains configuration values provided by the `directoryvalue` plugin
	 */
	explicit DirectoryValueDelegate (CppKeySet config);

	/**
	 * @brief This method converts all leaf keys in the given key set to directory keys.
	 *
	 * @param keys This parameter specifies the key set this function converts.
	 *
	 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the plugin converted any value in the given key set
	 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE if the plugin did not update `keys`
	 */
	int convertToDirectories (CppKeySet & keys);

	/**
	 * @brief This method converts all directory keys in the given key set to leaf keys.
	 *
	 * @param keys This parameter specifies the key set this function converts.
	 *
	 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the plugin converted any value in the given key set
	 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE if the plugin did not update `keys`
	 */
	int convertToLeaves (CppKeySet & keys);
};

} // end namespace elektra

#endif

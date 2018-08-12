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

#define DIRECTORY_POSTFIX "___dirdata"

namespace elektra
{
using std::pair;

using CppKeySet = kdb::KeySet;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

/**
 * @brief Split `keys` into two key sets, one for array parents and one for all other keys.
 *
 * @param keys This parameter contains the key set this function splits.
 *
 * @return A pair of key sets, where the first key set contains all array parents and the second key set contains all other keys
 */
pair<CppKeySet, CppKeySet> splitArrayParentsOther (CppKeySet const & keys);

/**
 * @brief Increase the array index of array elements in `arrays` by one.
 *
 * @param parents This parameter contains the array parents for which this function increases the index by one.
 * @param parents This variable stores the arrays elements this function modifies.
 *
 * @return A copy of `arrays`, where all indices specified by `parents` are increased by one.
 */
CppKeySet increaseArrayIndices (CppKeySet const & parents, CppKeySet const & arrays);

// -- Class --------------------------------------------------------------------------------------------------------------------------------

class LeafDelegate
{
public:
	/**
	 * @brief This constructor creates a new delegate object used by the `leaf` plugin
	 *
	 * @param config This key set contains configuration values provided by the `leaf` plugin
	 */
	explicit LeafDelegate (CppKeySet config);

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

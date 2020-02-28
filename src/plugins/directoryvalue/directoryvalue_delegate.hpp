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

#include <kdb.hpp>

#define DIRECTORY_POSTFIX "___dirdata"
#define ARRAY_VALUE_PREFIX "___dirdata:"

constexpr ssize_t arrayValuePrefixSize = sizeof (ARRAY_VALUE_PREFIX) - 1;

namespace elektra
{

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

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
std::pair<kdb::KeySet, kdb::KeySet> increaseArrayIndices (kdb::KeySet const & parents, kdb::KeySet const & arrays);

// -- Class --------------------------------------------------------------------------------------------------------------------------------

class DirectoryValueDelegate
{
public:
	/**
	 * @brief This constructor creates a new delegate object used by the `directoryvalue` plugin
	 *
	 * @param config This key set contains configuration values provided by the `directoryvalue` plugin
	 */
	explicit DirectoryValueDelegate (kdb::KeySet config);

	/**
	 * @brief This method converts all leaf keys in the given key set to directory keys.
	 *
	 * @param keys This parameter specifies the key set this function converts.
	 *
	 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the plugin converted any value in the given key set
	 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE if the plugin did not update `keys`
	 */
	int convertToDirectories (kdb::KeySet & keys);

	/**
	 * @brief This method converts all directory keys in the given key set to leaf keys.
	 *
	 * @param keys This parameter specifies the key set this function converts.
	 *
	 * @retval ELEKTRA_PLUGIN_STATUS_SUCCESS if the plugin converted any value in the given key set
	 * @retval ELEKTRA_PLUGIN_STATUS_NO_UPDATE if the plugin did not update `keys`
	 */
	int convertToLeaves (kdb::KeySet & keys);
};

} // end namespace elektra

#endif

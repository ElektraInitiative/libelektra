/**
 * @file
 *
 * @brief This file contains the declaration of a basic YAML to `KeySet`
 *        converter function.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAWN_CONVERT_HPP
#define ELEKTRA_PLUGIN_YAWN_CONVERT_HPP

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <kdb.hpp>

// -- Function -----------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/**
 * @brief This function converts the given YAML file to keys and adds the
 *        result to `keySet`.
 *
 * @param keySet The function adds the converted keys to this variable.
 * @param parent The function uses this parent key of `keySet` to emit error
 *               information.
 * @param filename This parameter stores the path of the YAML file this
 *                 function converts.
 *
 * @retval -2 if the file could not be opened for reading
 * @retval -1 if there was an error converting the YAML file
 * @retval  0 if parsing was successful and the function did not change the
 *            given key set
 * @retval  1 if parsing was successful and the function did change `keySet`
 */
int addToKeySet (kdb::KeySet & keySet, kdb::Key & parent, std::string const & filename);

} // namespace yawn

#endif // ELEKTRA_PLUGIN_YAWN_CONVER_HPP

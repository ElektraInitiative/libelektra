/**
 * @file
 *
 * @brief This file contains a function to convert a YAML file to a key set.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAYPEG_CONVERT_HPP
#define ELEKTRA_PLUGIN_YAYPEG_CONVERT_HPP

// -- Imports ------------------------------------------------------------------

#include <kdb.hpp>

// -- Function -----------------------------------------------------------------

namespace yaypeg
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
 * @retval  0 if parsing was successful and the function did not change the
 *            given keyset
 * @retval  1 if parsing was successful and the function did change `keySet`
 */
int addToKeySet (kdb::KeySet & keySet, kdb::Key & parent, std::string const & filename);

} // namespace yaypeg

#endif // ELEKTRA_PLUGIN_YAYPEG_CONVERT_HPP

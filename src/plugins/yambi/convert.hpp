/**
 * @file
 *
 * @brief This file contains a function to convert a YAML file to a key set.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAMBI_CONVERTER_HPP
#define ELEKTRA_PLUGIN_YAMBI_CONVERTER_HPP

// -- Imports ------------------------------------------------------------------

#include <kdb.hpp>

// -- Function -----------------------------------------------------------------

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
 * @retval -3 if the file could not be opened for reading
 * @retval -2 if parsing was unsuccessful due to memory exhaustion
 * @retval -1 if there was an syntax error converting the YAML file
 * @retval  0 if parsing was successful and the function did not change the given keyset
 * @retval  1 if parsing was successful and the function did change `keySet`
 */
int addToKeySet (kdb::KeySet & keySet, kdb::Key & parent, std::string const & filename);

#endif // ELEKTRA_PLUGIN_YAMBI_CONVERTER_HPP

/**
 * @file
 *
 * @brief Read key sets using yaml-cpp
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAMLCPP_READ_H
#define ELEKTRA_PLUGIN_YAMLCPP_READ_H

#include <keyset.hpp>

namespace yamlcpp
{
void yamlRead (kdb::KeySet & mappings, kdb::Key & parent);
}

#endif

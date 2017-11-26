/**
 * @file
 *
 * @brief Write key sets using yaml-cpp
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_YAMLCPP_WRITE_H
#define ELEKTRA_PLUGIN_YAMLCPP_WRITE_H

#include <keyset.hpp>

namespace yamlcpp
{
void yamlWrite (kdb::KeySet const & mappings, kdb::Key const & parent);
}

#endif

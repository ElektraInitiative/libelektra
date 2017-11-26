/**
 * @file
 *
 * @brief serialization logic for xerces plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_XERCES_SERIALIZER_H
#define ELEKTRA_PLUGIN_XERCES_SERIALIZER_H

#include <keyset.hpp>

namespace xerces
{

void serialize (kdb::Key const & parentKey, kdb::KeySet const & ks);
}

#endif

/**
 * @file
 *
 * @brief serialization logic for xerces plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_XERCES_SERIALIZER_H
#define ELEKTRA_PLUGIN_XERCES_SERIALIZER_H

#include <keyset.hpp>

void serialize (kdb::Key const & parentKey, kdb::KeySet const & ks);

#endif

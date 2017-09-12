/**
 * @file
 *
 * @brief deserialization logic for xerces plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_XERCES_DESERIALIZER_H
#define ELEKTRA_PLUGIN_XERCES_DESERIALIZER_H

#include <keyset.hpp>

namespace xerces
{
void deserialize (kdb::Key const & parentKey, kdb::KeySet & ks);
}

#endif

/**
 * @file
 *
 * @brief deserialization logic for xerces plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_XERCES_DESERIALIZER_H
#define ELEKTRA_PLUGIN_XERCES_DESERIALIZER_H

#include <keyset.hpp>

void deserialize (const std::string src, kdb::KeySet & ks);

#endif
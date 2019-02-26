/**
 * @file
 *
 * @brief Header for yambi plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_YAMBI_HPP
#define ELEKTRA_PLUGIN_YAMBI_HPP

#include <kdbplugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraYambiGet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraYambiSet (Plugin * handle, KeySet * returned, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;
} // end extern "C"

#endif

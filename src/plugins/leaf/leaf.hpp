/**
 * @file
 *
 * @brief Header for leaf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LEAF_HPP
#define ELEKTRA_PLUGIN_LEAF_HPP

#include <kdbplugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraLeafOpen (Plugin * handle, Key * errorKey);
int elektraLeafClose (Plugin * handle, Key * errorKey);
int elektraLeafGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLeafSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (leaf);
} // end extern "C"

#endif

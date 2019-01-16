/**
 * @file
 *
 * @brief Header for yaypeg plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_YAYPEG_HPP
#define ELEKTRA_PLUGIN_YAYPEG_HPP

#include <kdbplugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraYaypegOpen (Plugin * handle, Key * errorKey);
int elektraYaypegClose (Plugin * handle, Key * errorKey);
int elektraYaypegGet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraYaypegSet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraYaypegError (Plugin * handle, KeySet * conf, Key * parentKey);
int elektraYaypegCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (yaypeg);
} // end extern "C"

#endif

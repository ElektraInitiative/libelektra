/**
 * @file
 *
 * @brief Header for yawn plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_YAWN_HPP
#define ELEKTRA_PLUGIN_YAWN_HPP

#include <kdbplugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraYawnOpen (Plugin * handle, Key * errorKey);
int elektraYawnClose (Plugin * handle, Key * errorKey);
int elektraYawnGet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraYawnSet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraYawnError (Plugin * handle, KeySet * conf, Key * parentKey);
int elektraYawnCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (yawn);
} // end extern "C"

#endif

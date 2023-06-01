/**
 * @file
 *
 * @brief Header for kconfig plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_KCONFIG_HPP
#define ELEKTRA_PLUGIN_KCONFIG_HPP

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraKconfigOpen (Plugin * handle, Key * errorKey);
int elektraKconfigClose (Plugin * handle, Key * errorKey);
int elektraKconfigGet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraKconfigSet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraKconfigError (Plugin * handle, KeySet * conf, Key * parentKey);
int elektraKconfigCheckConf (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;
} // end extern "C"

#endif

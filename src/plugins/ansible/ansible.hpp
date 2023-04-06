/**
 * @file
 *
 * @brief Header for ansible plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_ANSIBLE_HPP
#define ELEKTRA_PLUGIN_ANSIBLE_HPP

#include <kdbplugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraAnsibleOpen (Plugin * handle, Key * errorKey);
int elektraAnsibleClose (Plugin * handle, Key * errorKey);
int elektraAnsibleGet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraAnsibleSet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraAnsibleError (Plugin * handle, KeySet * conf, Key * parentKey);
int elektraAnsibleCheckConf (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;
} // end extern "C"

#endif

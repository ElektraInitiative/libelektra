/**
 * @file
 *
 * @brief Header for yamlcpp plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_YAMLCPP_H
#define ELEKTRA_PLUGIN_YAMLCPP_H

#include <kdbplugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {

int elektraYamlcppGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraYamlcppSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT;

} // end extern "C"

#endif

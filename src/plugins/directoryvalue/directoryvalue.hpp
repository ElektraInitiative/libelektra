/**
 * @file
 *
 * @brief Header for directoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_DIRECTORY_VALUE_HPP
#define ELEKTRA_PLUGIN_DIRECTORY_VALUE_HPP

#include <elektra/plugin/plugin.h>

extern "C" {

int elektraDirectoryValueOpen (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraDirectoryValueClose (ckdb::Plugin * handle, ckdb::Key * errorKey);
int elektraDirectoryValueGet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);
int elektraDirectoryValueSet (ckdb::Plugin * handle, ckdb::KeySet * ks, ckdb::Key * parentKey);

ckdb::Plugin * ELEKTRA_PLUGIN_EXPORT;
} // end extern "C"

#endif

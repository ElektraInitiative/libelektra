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

#include <kdbplugin.h>

#include "directoryvalue_delegate.hpp"

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraDirectoryValueOpen (Plugin * handle, Key * errorKey);
int elektraDirectoryValueClose (Plugin * handle, Key * errorKey);
int elektraDirectoryValueGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDirectoryValueSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;
} // end extern "C"

#endif

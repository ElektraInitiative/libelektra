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

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <kdbplugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

extern "C" {
int elektraYawnGet (Plugin * handle, KeySet * returned, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (yawn);
} // end extern "C"

#endif

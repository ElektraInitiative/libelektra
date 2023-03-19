/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_CCODE_H
#define ELEKTRA_PLUGIN_CCODE_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraCcodeOpen (Plugin * handle, Key * key);
int elektraCcodeClose (Plugin * handle, Key * key);
int elektraCcodeGet (Plugin * handle, KeySet * keySet, Key * parentKey);
int elektraCcodeSet (Plugin * handle, KeySet * keySet, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;
} // end extern "C"

#endif

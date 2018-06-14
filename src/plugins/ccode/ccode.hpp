/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_CCODE_H
#define ELEKTRA_PLUGIN_CCODE_H

#include <kdbplugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraCcodeOpen (Plugin * handle, Key * k);
int elektraCcodeClose (Plugin * handle, Key * k);
int elektraCcodeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCcodeSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (ccode);
} // end extern "C"

#endif

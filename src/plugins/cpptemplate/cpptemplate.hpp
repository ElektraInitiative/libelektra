/**
 * @file
 *
 * @brief Header for cpptemplate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CPPTEMPLATE_HPP
#define ELEKTRA_PLUGIN_CPPTEMPLATE_HPP

#include <kdbplugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraCpptemplateOpen (Plugin * handle, Key * errorKey);
int elektraCpptemplateClose (Plugin * handle, Key * errorKey);
int elektraCpptemplateGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCpptemplateSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCpptemplateError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCpptemplateCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (cpptemplate);
} // end extern "C"

#endif

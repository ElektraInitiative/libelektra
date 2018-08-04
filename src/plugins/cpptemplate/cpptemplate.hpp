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
int elektraCppTemplateOpen (Plugin * handle, Key * errorKey);
int elektraCppTemplateClose (Plugin * handle, Key * errorKey);
int elektraCppTemplateGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCppTemplateSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCppTemplateError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCppTemplateCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (cpptemplate);
} // end extern "C"

#endif

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

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

extern "C" {
int elektraCppTemplateOpen (Plugin * handle, Key * errorKey);
int elektraCppTemplateClose (Plugin * handle, Key * errorKey);
int elektraCppTemplateGet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraCppTemplateSet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraCppTemplateError (Plugin * handle, KeySet * conf, Key * parentKey);
int elektraCppTemplateCheckConf (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT;
} // end extern "C"

#endif

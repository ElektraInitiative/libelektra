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

using ::Key;
using ::KeySet;
using ::Plugin;

extern "C" {

int elektraYamlcppGet (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);
int elektraYamlcppSet (::Plugin * handle, ::KeySet * ks, ::Key * parentKey);

::Plugin * ELEKTRA_PLUGIN_EXPORT;

} // end extern "C"

#endif

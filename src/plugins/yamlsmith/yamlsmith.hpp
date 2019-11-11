/**
 * @file
 *
 * @brief Header for yamlsmith plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_YAMLSMITH_H
#define ELEKTRA_PLUGIN_YAMLSMITH_H

#include <kdbplugin.h>

using ::Key;
using ::KeySet;
using ::Plugin;

extern "C" {

int elektraYamlsmithGet (Plugin *, KeySet *, Key *);
int elektraYamlsmithSet (Plugin *, KeySet *, Key *);

Plugin * ELEKTRA_PLUGIN_EXPORT;

} // end extern "C"


#endif

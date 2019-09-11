/**
 * @file
 *
 * @brief Header for camel plugin
 *
 * @copyright BSD License (see doc/LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CAMEL_H
#define ELEKTRA_PLUGIN_CAMEL_H

#include <kdbplugin.h>

int elektraCamelGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCamelSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

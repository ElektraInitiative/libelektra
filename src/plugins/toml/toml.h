/**                                                                                                                          
  1  * @file
  2  *
  3  * @brief Header for the toml plugin
  4  *
  5  * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
  6  *
  7  */

#ifndef ELEKTRA_PLUGIN_TOML_H
#define ELEKTRA_PLUGIN_TOML_H

#include <kdbplugin.h>

int elektraTomlGet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraTomlSet (Plugin * handle, KeySet * returned, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif  // ELEKTRA_PLUGIN_TOML_H

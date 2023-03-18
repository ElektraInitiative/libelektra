/**
 * @file
 *
 * @brief Header for conditionals plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_CONDITIONALS_H
#define ELEKTRA_PLUGIN_CONDITIONALS_H

#include <elektra/plugin/plugin.h>


int elektraConditionalsGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraConditionalsSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

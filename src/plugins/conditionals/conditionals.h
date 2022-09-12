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

#include <kdbplugin.h>


int elektraConditionalsGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraConditionalsSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

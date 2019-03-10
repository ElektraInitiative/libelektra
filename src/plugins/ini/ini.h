/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_INI_H
#define ELEKTRA_PLUGIN_INI_H

#include <kdbplugin.h>


int elektraIniGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraIniSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

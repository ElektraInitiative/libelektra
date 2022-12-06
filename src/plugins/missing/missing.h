/**
 * @file
 *
 * @brief Header for missing plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_MISSING_H
#define ELEKTRA_MISSING_H

#include <elektra/kdbplugin.h>

int elektraMissingGet (Plugin * plugin, KeySet * returned, Key * parentKey);
int elektraMissingSet (Plugin * plugin, KeySet * returned, Key * parentKey);
Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif // ELEKTRA_MISSING_H

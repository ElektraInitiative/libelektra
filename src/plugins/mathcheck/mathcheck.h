/**
 * @file
 *
 * @brief Header for calculate plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_MATHCHECK_H
#define ELEKTRA_PLUGIN_MATHCHECK_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>


int elektraMathcheckGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraMathcheckSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

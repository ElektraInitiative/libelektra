/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb/errors.h>
#include <elektra/plugin/plugin.h>


int elektraErrorGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraErrorSet (Plugin * handle, KeySet * ks, Key * parentKey);
Plugin * ELEKTRA_PLUGIN_EXPORT;

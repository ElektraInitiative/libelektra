/**
 * @file
 *
 * @brief Header for hexnumber plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_HEXNUMBER_H
#define ELEKTRA_HEXNUMBER_H

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>

#define ELEKTRA_HEXNUMBER_PLUGIN_NAME "hexnumber"
#define ELEKTRA_HEXNUMBER_META_KEY "internal/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/ishex"

int elektraHexnumberGet (Plugin * handle, KeySet * ks, Key * errorKey);
int elektraHexnumberSet (Plugin * handle, KeySet * ks, Key * errorKey);
int elektraHexnumberClose (Plugin * handle, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif // ELEKTRA_HEXNUMBER_H

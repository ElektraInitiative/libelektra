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

#include <kdbplugin.h>

#define ELEKTRA_HEXNUMBER_PLUGIN_NAME "hexnumber"
#define ELEKTRA_HEXNUMBER_META_KEY "internal/" ELEKTRA_HEXNUMBER_PLUGIN_NAME "/ishex"

int elektraHexnumberGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * errorKey);
int elektraHexnumberSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * errorKey);
int elektraHexnumberClose (Plugin * handle, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif // ELEKTRA_HEXNUMBER_H

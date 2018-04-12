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

#define ELEKTRA_HEXNUMBER_META_TYPE "hexnumber"

int elektraHexnumberGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraHexnumberSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (hexnumber);

#endif // ELEKTRA_HEXNUMBER_H

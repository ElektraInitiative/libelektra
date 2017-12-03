/**
 * @file
 *
 * @brief Header for dini plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_DINI_H
#define ELEKTRA_PLUGIN_DINI_H

#include <kdbplugin.h>


int elektraDiniOpen (Plugin * handle, Key * errorKey);
int elektraDiniClose (Plugin * handle, Key * errorKey);
int elektraDiniGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDiniSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDiniError (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDiniCheckConfig (Key * errorKey, KeySet * conf);

Plugin * ELEKTRA_PLUGIN_EXPORT (dini);

typedef struct
{
	KeySet * modules;
	Plugin * dump;
	Plugin * ini;
	Key * dumpErrors;
} Dini;

#endif

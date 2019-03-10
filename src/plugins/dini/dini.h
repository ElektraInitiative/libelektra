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

#include <kdbinvoke.h>
#include <kdbplugin.h>


int elektraDiniOpen (Plugin * handle, Key * errorKey);
int elektraDiniClose (Plugin * handle, Key * errorKey);
int elektraDiniGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraDiniSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

typedef struct
{
	KeySet * modules;
	ElektraInvokeHandle * dump;
	ElektraInvokeHandle * ini;
	ElektraInvokeHandle * bin;
	KeySet * dumpConfig;
	KeySet * iniConfig;
	Key * dumpErrors;
} Dini;

#endif

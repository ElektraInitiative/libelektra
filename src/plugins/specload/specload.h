/**
 * @file
 *
 * @brief Header for specload plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SPECLOAD_H
#define ELEKTRA_PLUGIN_SPECLOAD_H

#include <kdbinvoke.h>
#include <kdbplugin.h>

typedef struct
{
	char * app;
	char ** argv;
	KeySet * quickDumpConfig;
	ElektraInvokeHandle * quickDump;
} Specload;

int elektraSpecloadOpen (Plugin * handle, Key * errorKey);
int elektraSpecloadClose (Plugin * handle, Key * errorKey);
int elektraSpecloadGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSpecloadSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSpecloadCheckConfig (Key * errorKey, KeySet * conf);

int elektraSpecloadSendSpec (Plugin * handle, KeySet * spec, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

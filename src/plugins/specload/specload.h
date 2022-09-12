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
	char * directFile;
	char * app;
	char ** argv;
	ElektraKeyset * quickDumpConfig;
	ElektraInvokeHandle * quickDump;
} Specload;

int elektraSpecloadOpen (Plugin * handle, ElektraKey * errorKey);
int elektraSpecloadClose (Plugin * handle, ElektraKey * errorKey);
int elektraSpecloadGet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraSpecloadSet (Plugin * handle, ElektraKeyset * ks, ElektraKey * parentKey);
int elektraSpecloadCheckConf (ElektraKey * errorKey, ElektraKeyset * conf);

int elektraSpecloadSendSpec (Plugin * handle, ElektraKeyset * spec, ElektraKey * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

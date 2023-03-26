/**
 * @file
 *
 * @brief Header for spec plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SPEC_H
#define ELEKTRA_PLUGIN_SPEC_H

#include <kdbplugin.h>
#include <kdbtypes.h> /* for bool */

#ifndef ERROR_KEY
#define ERROR_KEY "meta:/error/spec"
#endif

#ifndef WARNING_KEY
#define WARNING_KEY "meta:/warning/spec"
#endif

#ifndef INFO_KEY
#define INFO_KEY "meta:/info/spec"
#endif

int elektraSpecGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSpecCopy (Plugin * handle, KeySet * returned, Key * parentKey, bool isKdbGet);
int elektraSpecRemove (Plugin * handle, KeySet * returned, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

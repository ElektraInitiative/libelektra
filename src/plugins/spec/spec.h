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

#include <elektra/core.h>
#include <elektra/plugin/plugin.h>
#include <elektra/type/types.h> /* for bool */

#ifndef INFO_KEY
#define INFO_KEY "info/spec"
#endif

int elektraSpecGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSpecCopy (Plugin * handle, KeySet * returned, Key * parentKey, bool isKdbGet);
int elektraSpecRemove (Plugin * handle, KeySet * returned, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#endif

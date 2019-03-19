/**
 * @file
 *
 * @brief Header file for entry points
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_NEWTYPE_H
#define ELEKTRA_PLUGIN_NEWTYPE_H

#include <stdbool.h>

#include <kdbplugin.h>

#ifdef __cplusplus
extern "C" {
namespace ckdb
{
#endif

typedef struct _Type Type;

int elektraNewTypeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNewTypeSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraNewTypeCheckConf (Key * errorKey, KeySet * conf);

bool elektraNewTypeCheckType (const Key * key);
bool elektraNewTypeValidateKey (Plugin * handle, Key * key, Key * errorKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#ifdef __cplusplus
}
};
#endif

#endif

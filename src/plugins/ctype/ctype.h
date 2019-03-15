/**
 * @file
 *
 * @brief Header file for entry points
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_CTYPE_H
#define ELEKTRA_PLUGIN_CTYPE_H

#include <stdbool.h>

#include <kdbplugin.h>

#ifdef __cplusplus
extern "C" {
namespace ckdb
{
#endif

typedef struct _Type Type;

int elektraCTypeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCTypeSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCTypeCheckConf (Key * errorKey, KeySet * conf);

bool elektraCTypeCheckType (const Key * key);
bool elektraCTypeValidateKey (Plugin * handle, Key * key, Key * errorKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#ifdef __cplusplus
}
};
#endif

#endif

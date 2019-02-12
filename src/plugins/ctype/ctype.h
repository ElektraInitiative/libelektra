/**
 * @file
 *
 * @brief Header file for entry points
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef ELEKTRA_PLUGIN_TYPE_H
#define ELEKTRA_PLUGIN_TYPE_H

#include <stdbool.h>

#include <kdbplugin.h>

#ifdef __cplusplus
extern "C" {
namespace ckdb
{
#endif

bool elektraCTypeCheckType (const Key * key);
int elektraCTypeGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraCTypeSet (Plugin * handle, KeySet * ks, Key * parentKey);
bool elektraCTypeValidateKey (Key * key, Key * errorKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (ctype);

#ifdef __cplusplus
}
};
#endif

#endif

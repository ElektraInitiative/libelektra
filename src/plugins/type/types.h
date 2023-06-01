/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_TYPE_TYPES_H
#define ELEKTRA_PLUGIN_TYPE_TYPES_H

#include <stdbool.h>

#include <elektra/core.h>
#include <elektra/core/key.h>
#include <elektra/plugin/plugin.h>
#include <elektra/type.h>

bool elektraTypeCheckAny (const Key * key);
bool elektraTypeCheckEmpty (const Key * key);
bool elektraTypeCheckChar (const Key * key);
bool elektraTypeCheckWChar (const Key * key);
bool elektraTypeCheckString (const Key * key);
bool elektraTypeCheckWString (const Key * key);

bool elektraTypeNormalizeBoolean (Plugin * handle, Key * key);
bool elektraTypeCheckBoolean (const Key * key);
bool elektraTypeRestoreBoolean (Plugin * handle, Key * key);

bool elektraTypeCheckFloat (const Key * key);
bool elektraTypeCheckDouble (const Key * key);
bool elektraTypeCheckLongDouble (const Key * key);

bool elektraTypeCheckShort (const Key * key);
bool elektraTypeCheckLong (const Key * key);
bool elektraTypeCheckLongLong (const Key * key);
bool elektraTypeCheckUnsignedShort (const Key * key);
bool elektraTypeCheckUnsignedLong (const Key * key);
bool elektraTypeCheckUnsignedLongLong (const Key * key);

bool elektraTypeNormalizeEnum (Plugin * handle, Key * key);
bool elektraTypeCheckEnum (const Key * key);
bool elektraTypeRestoreEnum (Plugin * handle, Key * key);
void elektraTypeSetErrorEnum (Plugin * handle, Key * errorKey, const Key * key);

#endif // ELEKTRA_PLUGIN_TYPE_TYPES_H

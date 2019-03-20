/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_NEWTYPE_TYPES_H
#define ELEKTRA_NEWTYPE_TYPES_H

#include <stdbool.h>

#include <kdb.h>
#include <kdbplugin.h>
#include <kdbtypes.h>

bool elektraNewTypeCheckAny (const Key * key);
bool elektraNewTypeCheckEmpty (const Key * key);
bool elektraNewTypeCheckChar (const Key * key);
bool elektraNewTypeCheckWChar (const Key * key);
bool elektraNewTypeCheckString (const Key * key);
bool elektraNewTypeCheckWString (const Key * key);

bool elektraNewTypeNormalizeBoolean (Plugin * handle, Key * key);
bool elektraNewTypeCheckBoolean (const Key * key);
bool elektraNewTypeRestoreBoolean (Plugin * handle, Key * key);

bool elektraNewTypeCheckFloat (const Key * key);
bool elektraNewTypeCheckDouble (const Key * key);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
bool elektraNewTypeCheckLongDouble (const Key * key);
#endif

bool elektraNewTypeCheckShort (const Key * key);
bool elektraNewTypeCheckLong (const Key * key);
bool elektraNewTypeCheckLongLong (const Key * key);
bool elektraNewTypeCheckUnsignedShort (const Key * key);
bool elektraNewTypeCheckUnsignedLong (const Key * key);
bool elektraNewTypeCheckUnsignedLongLong (const Key * key);

bool elektraNewTypeNormalizeEnum (Plugin * handle, Key * key);
bool elektraNewTypeCheckEnum (const Key * key);
bool elektraNewTypeRestoreEnum (Plugin * handle, Key * key);
void elektraNewTypeSetErrorEnum (Plugin * handle, Key * errorKey, const Key * key);

#endif

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_TYPE_TYPES_H
#define ELEKTRA_TYPE_TYPES_H

#include <stdbool.h>

#include <kdb.h>
#include <kdbplugin.h>
#include <kdbtypes.h>

bool elektraTypeCheckAny (const ElektraKey * key);
bool elektraTypeCheckEmpty (const ElektraKey * key);
bool elektraTypeCheckChar (const ElektraKey * key);
bool elektraTypeCheckWChar (const ElektraKey * key);
bool elektraTypeCheckString (const ElektraKey * key);
bool elektraTypeCheckWString (const ElektraKey * key);

bool elektraTypeNormalizeBoolean (Plugin * handle, ElektraKey * key);
bool elektraTypeCheckBoolean (const ElektraKey * key);
bool elektraTypeRestoreBoolean (Plugin * handle, ElektraKey * key);

bool elektraTypeCheckFloat (const ElektraKey * key);
bool elektraTypeCheckDouble (const ElektraKey * key);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
bool elektraTypeCheckLongDouble (const ElektraKey * key);
#endif

bool elektraTypeCheckShort (const ElektraKey * key);
bool elektraTypeCheckLong (const ElektraKey * key);
bool elektraTypeCheckLongLong (const ElektraKey * key);
bool elektraTypeCheckUnsignedShort (const ElektraKey * key);
bool elektraTypeCheckUnsignedLong (const ElektraKey * key);
bool elektraTypeCheckUnsignedLongLong (const ElektraKey * key);

bool elektraTypeNormalizeEnum (Plugin * handle, ElektraKey * key);
bool elektraTypeCheckEnum (const ElektraKey * key);
bool elektraTypeRestoreEnum (Plugin * handle, ElektraKey * key);
void elektraTypeSetErrorEnum (Plugin * handle, ElektraKey * errorKey, const ElektraKey * key);

#endif

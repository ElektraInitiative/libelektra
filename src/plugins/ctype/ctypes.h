/**
 * @file
 *
 * @brief Implementation of data types
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_CTYPES_HPP
#define ELEKTRA_CTYPES_HPP

#include <stdbool.h>

#include <kdb.h>
#include <kdbplugin.h>
#include <kdbtypes.h>

bool elektraCTypeCheckAny (const Key * key);
bool elektraCTypeCheckEmpty (const Key * key);
bool elektraCTypeCheckChar (const Key * key);
bool elektraCTypeCheckWChar (const Key * key);
bool elektraCTypeCheckString (const Key * key);
bool elektraCTypeCheckWString (const Key * key);

bool elektraCTypeNormalizeBoolean (Plugin * handle, Key * key);
bool elektraCTypeCheckBoolean (const Key * key);
bool elektraCTypeRestoreBoolean (Plugin * handle, Key * key);

bool elektraCTypeCheckFloat (const Key * key);
bool elektraCTypeCheckDouble (const Key * key);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE
bool elektraCTypeCheckLongDouble (const Key * key);
#endif

bool elektraCTypeCheckShort (const Key * key);
bool elektraCTypeCheckLong (const Key * key);
bool elektraCTypeCheckLongLong (const Key * key);
bool elektraCTypeCheckUnsignedShort (const Key * key);
bool elektraCTypeCheckUnsignedLong (const Key * key);
bool elektraCTypeCheckUnsignedLongLong (const Key * key);

bool elektraCTypeCheckEnum (const Key * key);

#endif

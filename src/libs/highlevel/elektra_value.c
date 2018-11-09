/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra.h"
#include "elektra_conversion.h"
#include "elektra_error_private.h"
#include "elektra_private.h"
#include "kdblogger.h"
#include <elektra.h>
#include <string.h>


/**
 * \defgroup highlevel High-level API
 * @{
 */

Key * elektraFindKey (Elektra * elektra, const char * name, KDBType type)
{
	elektraSetLookupKey (elektra, name);
	Key * const resultKey = ksLookup (elektra->config, elektra->lookupKey, 0);
	if (resultKey == NULL)
	{
		elektraFatalError (elektra, elektraErrorKeyNotFound (keyName (elektra->lookupKey), NULL));
	}

	if (!elektra->enforceType && type != NULL)
	{
		const char * actualType = keyString (keyGetMeta (resultKey, "type"));
		if (strcmp (actualType, type) != 0)
		{
			elektraFatalError (elektra, elektraErrorWrongType (keyName (elektra->lookupKey), actualType, type, NULL));
		}
	}

	return resultKey;
}

const char * elektraGetValue (Elektra * elektra, const char * name)
{
	return keyString (elektraFindKey (elektra, name, NULL));
}

void elektraSetValue (Elektra * elektra, const char * name, const char * value, KDBType type, ElektraError ** error)
{
	elektraSetLookupKey (elektra, name);
	Key * const key = keyDup (elektra->lookupKey);
	keySetMeta (key, "type", type);
	keySetString (key, value);
	elektraSaveKey (elektra, key, error);
}

#define ELEKTRA_GET_VALUE(KEY_TO_VALUE, KDB_TYPE, elektra, keyname, result)                                                                \
	const Key * key = elektraFindKey (elektra, keyname, KDB_TYPE);                                                                     \
	if (!KEY_TO_VALUE (key, &result))                                                                                                  \
	{                                                                                                                                  \
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE, keyString (key), NULL));                           \
	}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyname The (relative) keyname to look up. The keyname is appended to the parent key.
 * @return The value stored at the given key and index.
 */
const char * elektraGetString (Elektra * elektra, const char * keyname)
{
	const char * result;
	ELEKTRA_GET_VALUE (elektraKeyToString, KDB_TYPE_STRING, elektra, keyname, result);
	return result;
}

kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * keyname)
{
	kdb_boolean_t result;
	ELEKTRA_GET_VALUE (elektraKeyToBoolean, KDB_TYPE_BOOLEAN, elektra, keyname, result);
	return result;
}

kdb_char_t elektraGetChar (Elektra * elektra, const char * keyname)
{
	kdb_char_t result;
	ELEKTRA_GET_VALUE (elektraKeyToChar, KDB_TYPE_CHAR, elektra, keyname, result);
	return result;
}

kdb_octet_t elektraGetOctet (Elektra * elektra, const char * keyname)
{
	kdb_octet_t result;
	ELEKTRA_GET_VALUE (elektraKeyToOctet, KDB_TYPE_OCTET, elektra, keyname, result);
	return result;
}

kdb_short_t elektraGetShort (Elektra * elektra, const char * keyname)
{
	kdb_short_t result;
	ELEKTRA_GET_VALUE (elektraKeyToShort, KDB_TYPE_SHORT, elektra, keyname, result);
	return result;
}

kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * keyname)
{
	kdb_unsigned_short_t result;
	ELEKTRA_GET_VALUE (elektraKeyToUnsignedShort, KDB_TYPE_UNSIGNED_SHORT, elektra, keyname, result);
	return result;
}

kdb_long_t elektraGetLong (Elektra * elektra, const char * keyname)
{
	kdb_long_t result;
	ELEKTRA_GET_VALUE (elektraKeyToLong, KDB_TYPE_LONG, elektra, keyname, result);
	return result;
}

kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * keyname)
{
	kdb_unsigned_long_t result;
	ELEKTRA_GET_VALUE (elektraKeyToUnsignedLong, KDB_TYPE_UNSIGNED_LONG, elektra, keyname, result);
	return result;
}

kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * keyname)
{
	kdb_long_long_t result;
	ELEKTRA_GET_VALUE (elektraKeyToLongLong, KDB_TYPE_LONG_LONG, elektra, keyname, result);
	return result;
}

kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * keyname)
{
	kdb_unsigned_long_long_t result;
	ELEKTRA_GET_VALUE (elektraKeyToUnsignedLongLong, KDB_TYPE_UNSIGNED_LONG_LONG, elektra, keyname, result);
	return result;
}

kdb_float_t elektraGetFloat (Elektra * elektra, const char * keyname)
{
	kdb_float_t result;
	ELEKTRA_GET_VALUE (elektraKeyToFloat, KDB_TYPE_FLOAT, elektra, keyname, result);
	return result;
}

kdb_double_t elektraGetDouble (Elektra * elektra, const char * keyname)
{
	kdb_double_t result;
	ELEKTRA_GET_VALUE (elektraKeyToDouble, KDB_TYPE_DOUBLE, elektra, keyname, result);
	return result;
}

kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * keyname)
{
	kdb_long_double_t result;
	ELEKTRA_GET_VALUE (elektraKeyToLongDouble, KDB_TYPE_LONG_DOUBLE, elektra, keyname, result);
	return result;
}

int elektraGetEnumInt (Elektra * elektra, char * keyname)
{
	int result;
	ELEKTRA_GET_VALUE (elektraKeyToLong, KDB_TYPE_ENUM, elektra, keyname, result);
	return result;
}

#define ELEKTRA_SET_VALUE(VALUE_TO_STRING, KDB_TYPE, elektra, keyname, value, error)                                                       \
	char * string = VALUE_TO_STRING (value);                                                                                           \
	if (string == 0)                                                                                                                   \
	{                                                                                                                                  \
		*error = elektraErrorConversionToString (KDB_TYPE, NULL);                                                                  \
		return;                                                                                                                    \
	}                                                                                                                                  \
	elektraSetValue (elektra, keyname, string, KDB_TYPE, error);                                                                       \
	elektraFree (string);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyname The (relative) keyname to write to. The keyname is appended to the parent key.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 */
void elektraSetString (Elektra * elektra, const char * keyName, const char * value, ElektraError ** error)
{
	elektraSetValue (elektra, keyName, value, KDB_TYPE_STRING, error);
}

void elektraSetBoolean (Elektra * elektra, const char * keyname, kdb_boolean_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraBooleanToString, KDB_TYPE_BOOLEAN, elektra, keyname, value, error);
}

void elektraSetChar (Elektra * elektra, const char * keyname, kdb_char_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraCharToString, KDB_TYPE_CHAR, elektra, keyname, value, error);
}

void elektraSetOctet (Elektra * elektra, const char * keyname, kdb_octet_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraOctetToString, KDB_TYPE_OCTET, elektra, keyname, value, error);
}

void elektraSetShort (Elektra * elektra, const char * keyname, kdb_short_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraShortToString, KDB_TYPE_SHORT, elektra, keyname, value, error);
}

void elektraSetUnsignedShort (Elektra * elektra, const char * keyname, kdb_unsigned_short_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraUnsignedShortToString, KDB_TYPE_UNSIGNED_SHORT, elektra, keyname, value, error);
}

void elektraSetLong (Elektra * elektra, const char * keyname, kdb_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraLongToString, KDB_TYPE_LONG, elektra, keyname, value, error);
}

void elektraSetUnsignedLong (Elektra * elektra, const char * keyname, kdb_unsigned_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraUnsignedLongToString, KDB_TYPE_UNSIGNED_LONG, elektra, keyname, value, error);
}

void elektraSetLongLong (Elektra * elektra, const char * keyname, kdb_long_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraLongLongToString, KDB_TYPE_LONG_LONG, elektra, keyname, value, error);
}

void elektraSetUnsignedLongLong (Elektra * elektra, const char * keyname, kdb_unsigned_long_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraUnsignedLongLongToString, KDB_TYPE_UNSIGNED_LONG_LONG, elektra, keyname, value, error);
}

void elektraSetFloat (Elektra * elektra, const char * keyname, kdb_float_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraFloatToString, KDB_TYPE_FLOAT, elektra, keyname, value, error);
}

void elektraSetDouble (Elektra * elektra, const char * keyname, kdb_double_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraDoubleToString, KDB_TYPE_DOUBLE, elektra, keyname, value, error);
}

void elektraSetLongDouble (Elektra * elektra, const char * keyname, kdb_long_double_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraLongDoubleToString, KDB_TYPE_LONG_DOUBLE, elektra, keyname, value, error);
}

void elektraSetEnumInt (Elektra * elektra, char * keyName, int value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraLongToString, KDB_TYPE_ENUM, elektra, keyName, value, error);
}

/**
 * @}
 */

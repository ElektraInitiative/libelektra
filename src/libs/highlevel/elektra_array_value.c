/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra.h"
#include "elektra_conversion.h"
#include "elektra_private.h"
#include "kdbease.h"
#include "kdblogger.h"

#include <elektra_error_private.h>
#include <stdlib.h>
#include <string.h>

/**
 * \defgroup highlevel High-level API
 * @{
 */

size_t elektraArraySize (Elektra * elektra, const char * name)
{
	elektraSetLookupKey (elektra, name);
	KeySet * arrayKeys = elektraArrayGet (elektra->lookupKey, elektra->config);
	size_t size = (size_t) ksGetSize (arrayKeys);
	ksDel (arrayKeys);

	return size;
}

Key * elektraFindArrayElementKey (Elektra * elektra, const char * name, size_t index, KDBType type)
{
	elektraSetArrayLookupKey (elektra, name, index);
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

const char * elektraGetArrayElementValue (Elektra * elektra, const char * name, size_t index)
{
	return keyString (elektraFindArrayElementKey (elektra, name, index, NULL));
}

void elektraSetArrayElementValue (Elektra * elektra, const char * name, size_t index, const char * value, KDBType type,
				  ElektraError ** error)
{
	elektraSetArrayLookupKey (elektra, name, index);
	Key * const key = keyDup (elektra->lookupKey);
	keySetMeta (key, "type", type);
	keySetString (key, value);

	elektraSaveKey (elektra, key, error);
}

#define ELEKTRA_GET_ARRAY_ELEMENT_VALUE(KEY_TO_VALUE, KDB_TYPE, elektra, keyname, index, result)                                           \
	const Key * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE);                                                  \
	if (!KEY_TO_VALUE (key, &result))                                                                                                  \
	{                                                                                                                                  \
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE, keyString (key), NULL));                           \
	}

const char * elektraGetStringArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	const char * result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToString, KDB_TYPE_STRING, elektra, keyname, index, result);
	return result;
}

kdb_boolean_t elektraGetBooleanArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_boolean_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToBoolean, KDB_TYPE_BOOLEAN, elektra, keyname, index, result);
	return result;
}

kdb_char_t elektraGetCharArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_char_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToChar, KDB_TYPE_CHAR, elektra, keyname, index, result);
	return result;
}

kdb_octet_t elektraGetOctetArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_octet_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToOctet, KDB_TYPE_OCTET, elektra, keyname, index, result);
	return result;
}

kdb_short_t elektraGetShortArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_short_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToShort, KDB_TYPE_SHORT, elektra, keyname, index, result);
	return result;
}

kdb_unsigned_short_t elektraGetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_unsigned_short_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToUnsignedShort, KDB_TYPE_UNSIGNED_SHORT, elektra, keyname, index, result);
	return result;
}

kdb_long_t elektraGetLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_long_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToLong, KDB_TYPE_LONG, elektra, keyname, index, result);
	return result;
}

kdb_unsigned_long_t elektraGetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_unsigned_long_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToUnsignedLong, KDB_TYPE_UNSIGNED_LONG, elektra, keyname, index, result);
	return result;
}

kdb_long_long_t elektraGetLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_long_long_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToLongLong, KDB_TYPE_LONG_LONG, elektra, keyname, index, result);
	return result;
}

kdb_unsigned_long_long_t elektraGetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_unsigned_long_long_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToUnsignedLongLong, KDB_TYPE_UNSIGNED_LONG_LONG, elektra, keyname, index, result);
	return result;
}

kdb_float_t elektraGetFloatArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_float_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToFloat, KDB_TYPE_FLOAT, elektra, keyname, index, result);
	return result;
}

kdb_double_t elektraGetDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_double_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToDouble, KDB_TYPE_DOUBLE, elektra, keyname, index, result);
	return result;
}

kdb_long_double_t elektraGetLongDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_long_double_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToLongDouble, KDB_TYPE_LONG_DOUBLE, elektra, keyname, index, result);
	return result;
}

int elektraGetEnumIntArrayElement (Elektra * elektra, char * keyname, size_t index)
{
	int result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToLong, KDB_TYPE_ENUM, elektra, keyname, index, result);
	return result;
}

#define ELEKTRA_SET_ARRAY_ELEMENT_VALUE(VALUE_TO_STRING, KDB_TYPE, elektra, keyname, index, value, error)                                  \
	char * string = VALUE_TO_STRING (value);                                                                                           \
	if (string == 0)                                                                                                                   \
	{                                                                                                                                  \
		*error = elektraErrorConversionToString (KDB_TYPE, NULL);                                                                  \
		return;                                                                                                                    \
	}                                                                                                                                  \
	elektraSetArrayElementValue (elektra, keyname, index, string, KDB_TYPE, error);                                                    \
	elektraFree (string);

void elektraSetStringArrayElement (Elektra * elektra, const char * keyname, size_t index, const char * value, ElektraError ** error)
{
	elektraSetArrayElementValue (elektra, keyname, index, value, KDB_TYPE_STRING, error);
}

void elektraSetBooleanArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_boolean_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraBooleanToString, KDB_TYPE_BOOLEAN, elektra, keyname, index, value, error);
}

void elektraSetCharArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_char_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraCharToString, KDB_TYPE_CHAR, elektra, keyname, index, value, error);
}

void elektraSetOctetArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_octet_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraOctetToString, KDB_TYPE_OCTET, elektra, keyname, index, value, error);
}

void elektraSetShortArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_short_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraShortToString, KDB_TYPE_SHORT, elektra, keyname, index, value, error);
}

void elektraSetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_short_t value,
					  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraUnsignedShortToString, KDB_TYPE_UNSIGNED_SHORT, elektra, keyname, index, value, error);
}

void elektraSetLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraLongToString, KDB_TYPE_LONG, elektra, keyname, index, value, error);
}

void elektraSetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_long_t value,
					 ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraUnsignedLongToString, KDB_TYPE_UNSIGNED_LONG, elektra, keyname, index, value, error);
}

void elektraSetLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraLongLongToString, KDB_TYPE_LONG_LONG, elektra, keyname, index, value, error);
}

void elektraSetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_long_long_t value,
					     ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraUnsignedLongLongToString, KDB_TYPE_UNSIGNED_LONG_LONG, elektra, keyname, index, value,
					 error);
}

void elektraSetFloatArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_float_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraFloatToString, KDB_TYPE_FLOAT, elektra, keyname, index, value, error);
}

void elektraSetDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_double_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraDoubleToString, KDB_TYPE_DOUBLE, elektra, keyname, index, value, error);
}

void elektraSetLongDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_double_t value,
				       ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraLongDoubleToString, KDB_TYPE_LONG_DOUBLE, elektra, keyname, index, value, error);
}

void elektraSetEnumIntArrayElement (Elektra * elektra, char * keyName, size_t index, int value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraLongToString, KDB_TYPE_ENUM, elektra, keyName, index, value, error);
}

/**
 * @}
 */

// Private functions
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
#include "kdbprivate.h"
#include <memory.h>

#include "stdio.h"

KDBType KDB_TYPE_STRING = "string";
KDBType KDB_TYPE_BOOLEAN = "boolean";
KDBType KDB_TYPE_CHAR = "char";
KDBType KDB_TYPE_OCTET = "octet";
KDBType KDB_TYPE_SHORT = "short";
KDBType KDB_TYPE_UNSIGNED_SHORT = "unsigned_short";
KDBType KDB_TYPE_LONG = "long";
KDBType KDB_TYPE_UNSIGNED_LONG = "unsigned_long";
KDBType KDB_TYPE_LONG_LONG = "long_long";
KDBType KDB_TYPE_UNSIGNED_LONG_LONG = "unsigned_long_long";
KDBType KDB_TYPE_FLOAT = "float";
KDBType KDB_TYPE_LONG_DOUBLE = "long_double";
KDBType KDB_TYPE_DOUBLE = "double";
KDBType KDB_TYPE_ENUM = "enum";

static void setLookupKey (Elektra * elektra, const char * name);
static void setArrayLookupKey (Elektra * elektra, const char * name, size_t index);
static void saveKey (Elektra * elektra, Key * key, ElektraError ** error);
static void checkType (Key * key, KDBType type);

/**
 * \defgroup highlevel High-level API
 * @{
 */

ELEKTRA_TAG_DEFINITIONS (const char *, String, KDB_TYPE_STRING, elektraStrDup, elektraKeyToString)
ELEKTRA_TAG_DEFINITIONS (kdb_boolean_t, Boolean, KDB_TYPE_BOOLEAN, elektraBooleanToString, elektraKeyToBoolean)
ELEKTRA_TAG_DEFINITIONS (kdb_char_t, Char, KDB_TYPE_CHAR, elektraCharToString, elektraKeyToChar)
ELEKTRA_TAG_DEFINITIONS (kdb_octet_t, Octet, KDB_TYPE_OCTET, elektraOctetToString, elektraKeyToOctet)
ELEKTRA_TAG_DEFINITIONS (kdb_short_t, Short, KDB_TYPE_SHORT, elektraShortToString, elektraKeyToShort)
ELEKTRA_TAG_DEFINITIONS (kdb_unsigned_short_t, UnsignedShort, KDB_TYPE_UNSIGNED_SHORT, elektraUnsignedShortToString,
			 elektraKeyToUnsignedShort)
ELEKTRA_TAG_DEFINITIONS (kdb_long_t, Long, KDB_TYPE_LONG, elektraLongToString, elektraKeyToLong)
ELEKTRA_TAG_DEFINITIONS (kdb_unsigned_long_t, UnsignedLong, KDB_TYPE_UNSIGNED_LONG, elektraUnsignedLongToString, elektraKeyToUnsignedLong)
ELEKTRA_TAG_DEFINITIONS (kdb_long_long_t, LongLong, KDB_TYPE_LONG_LONG, elektraLongLongToString, elektraKeyToLongLong)
ELEKTRA_TAG_DEFINITIONS (kdb_unsigned_long_long_t, UnsignedLongLong, KDB_TYPE_UNSIGNED_LONG_LONG, elektraLongLongToString,
			 elektraKeyToUnsignedLongLong)
ELEKTRA_TAG_DEFINITIONS (kdb_float_t, Float, KDB_TYPE_FLOAT, elektraFloatToString, elektraKeyToFloat)
ELEKTRA_TAG_DEFINITIONS (kdb_double_t, Double, KDB_TYPE_DOUBLE, elektraDoubleToString, elektraKeyToDouble)
ELEKTRA_TAG_DEFINITIONS (kdb_long_double_t, LongDouble, KDB_TYPE_LONG_DOUBLE, elektraLongDoubleToString, elektraKeyToLongDouble)

Elektra * elektraOpen (const char * application, KeySet * defaults, ElektraError ** error)
{
	Key * const parentKey = keyNew (application, KEY_END);
	KDB * const kdb = kdbOpen (parentKey);

	if (kdb == NULL)
	{
		*error = elektraErrorCreateFromKey (parentKey);
		return NULL;
	}

	KeySet * const config = ksNew (0, KS_END);
	if (defaults != NULL)
	{
		ksRewind (defaults);
		for (Key * key = ksNext (defaults); key != NULL; key = ksNext (defaults))
		{
			Key * const dup = keyDup (key);
			const char * name = keyName (key);
			keySetName (dup, keyName (parentKey));
			keyAddName (dup, name);
			ksAppendKey (config, dup);
		}
	}

	const int kdbGetResult = kdbGet (kdb, config, parentKey);

	if (kdbGetResult == -1)
	{
		*error = elektraErrorCreateFromKey (parentKey);
		return NULL;
	}

	Elektra * const elektra = elektraCalloc (sizeof (struct _Elektra));
	elektra->kdb = kdb;
	elektra->parentKey = parentKey;
	elektra->config = config;
	elektra->lookupKey = keyNew (NULL, KEY_END);

	return elektra;
}

void elektraClose (Elektra * elektra)
{
	kdbClose (elektra->kdb, elektra->parentKey);
	keyDel (elektra->parentKey);
	ksDel (elektra->config);
	keyDel (elektra->lookupKey);

	elektraFree (elektra);
}

size_t elektraArraySize (Elektra * elektra, const char * name)
{
	setLookupKey (elektra, name);
	KeySet * arrayKeys = elektraArrayGet (elektra->lookupKey, elektra->config);
	size_t size = (size_t) ksGetSize (arrayKeys);
	ksDel (arrayKeys);

	return size;
}

Key * elektraFindKey (Elektra * elektra, const char * name, KDBType type)
{
	setLookupKey (elektra, name);
	Key * const resultKey = ksLookup (elektra->config, elektra->lookupKey, 0);
	if (resultKey == NULL)
	{
		ELEKTRA_LOG_DEBUG ("Key not found: %s\n", keyName (elektra->lookupKey));
		exit (EXIT_FAILURE);
	}

	if (type != NULL)
	{
		checkType (resultKey, type);
	}

	return resultKey;
}

Key * elektraFindArrayElementKey (Elektra * elektra, const char * name, size_t index, KDBType type)
{
	setArrayLookupKey (elektra, name, index);
	Key * const resultKey = ksLookup (elektra->config, elektra->lookupKey, 0);
	if (resultKey == NULL)
	{
		ELEKTRA_LOG_DEBUG ("Key not found: %s\n", keyName (elektra->lookupKey));
		exit (EXIT_FAILURE);
	}

	if (type != NULL)
	{
		checkType (resultKey, type);
	}

	return resultKey;
}

const char * elektraGetValue (Elektra * elektra, const char * name)
{
	return keyString (elektraFindKey (elektra, name, NULL));
}

const char * elektraGetArrayElementValue (Elektra * elektra, const char * name, size_t index)
{
	return keyString (elektraFindArrayElementKey (elektra, name, index, NULL));
}

void elektraSetValue (Elektra * elektra, const char * name, const char * value, KDBType type, ElektraError ** error)
{
	setLookupKey (elektra, name);
	Key * const key = keyDup (elektra->lookupKey);
	keySetMeta (key, "type", type);
	keySetString (key, value);

	saveKey (elektra, key, error);
}

void elektraSetArrayElementValue (Elektra * elektra, const char * name, size_t index, const char * value, KDBType type,
				  ElektraError ** error)
{
	setArrayLookupKey (elektra, name, index);
	Key * const key = keyDup (elektra->lookupKey);
	keySetMeta (key, "type", type);
	keySetString (key, value);

	saveKey (elektra, key, error);
}

#define ELEKTRA_GET_VALUE(KEY_TO_VALUE, KDB_TYPE, elektra, keyname, result)                                                                \
	const Key * key = elektraFindKey (elektra, keyname, KDB_TYPE);                                                                     \
	if (!KEY_TO_VALUE (key, &result))                                                                                                  \
	{                                                                                                                                  \
		ELEKTRA_LOG_DEBUG ("Could not convert key to %s: %s\n", KDB_TYPE, keyname);                                                \
		exit (EXIT_FAILURE);                                                                                                       \
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

#define ELEKTRA_GET_ARRAY_ELEMENT_VALUE(KEY_TO_VALUE, KDB_TYPE, elektra, keyname, index, result)                                           \
	const Key * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE);                                                  \
	if (!KEY_TO_VALUE (key, &result))                                                                                                  \
	{                                                                                                                                  \
		ELEKTRA_LOG_DEBUG ("Could not convert key to %s: %s\n", KDB_TYPE, keyname);                                                \
		exit (EXIT_FAILURE);                                                                                                       \
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

/**
 * @}
 */

// Private functions

static void setLookupKey (Elektra * elektra, const char * name)
{
	keySetName (elektra->lookupKey, keyName (elektra->parentKey));
	keyAddName (elektra->lookupKey, name);
}

static void setArrayLookupKey (Elektra * elektra, const char * name, size_t index)
{
	setLookupKey (elektra, name);
	char arrayPart[ELEKTRA_MAX_ARRAY_SIZE];
	elektraWriteArrayNumber (arrayPart, index);
	keyAddName (elektra->lookupKey, arrayPart);
}

void saveKey (Elektra * elektra, Key * key, ElektraError ** error)
{
	int ret = 0;
	do
	{
		ksAppendKey (elektra->config, key);

		ret = kdbSet (elektra->kdb, elektra->config, elektra->parentKey);
		if (ret == -1)
		{
			ElektraError * kdbSetError = elektraErrorCreateFromKey (elektra->parentKey);
			if (elektraErrorCode (kdbSetError) != 30) // ELEKTRA_ERROR_CONFLICT = 30
			{
				*error = kdbSetError;
				return;
			}

			Key * problemKey = ksCurrent (elektra->config);
			if (problemKey != NULL)
			{
				ELEKTRA_LOG_DEBUG ("problemKey: %s\n", keyName (problemKey));
			}

			key = keyDup (key);
			kdbGet (elektra->kdb, elektra->config, elektra->parentKey);
		}
	} while (ret == -1);
}

void checkType (Key * key, KDBType type)
{
	if (strcmp (keyString (keyGetMeta (key, "type")), type) != 0)
	{
		ELEKTRA_LOG_DEBUG ("Wrong type. Should be: %s\n", type);
		exit (EXIT_FAILURE);
	}
}

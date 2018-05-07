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

static Key * generateLookupKey (Elektra * elektra, const char * name);
static Key * generateArrayLookupKey (Elektra * elektra, const char * name, size_t index);

static const char * getKeyValue (Elektra * elektra, Key * key, KDBType type);
static void setKeyValue (Elektra * elektra, Key * key, KDBType type, const char * value, ElektraError ** error);

/**
 * \defgroup highlevel High-level API
 * @{
 */


ELEKTRA_TAG_DEFINITIONS (const char *, String, KDB_TYPE_STRING, KDB_STRING_TO_STRING, KDB_STRING_TO_STRING)
ELEKTRA_TAG_DEFINITIONS (kdb_boolean_t, Boolean, KDB_TYPE_BOOLEAN, KDB_BOOLEAN_TO_STRING, KDB_STRING_TO_BOOLEAN)
ELEKTRA_TAG_DEFINITIONS (kdb_char_t, Char, KDB_TYPE_CHAR, KDB_CHAR_TO_STRING, KDB_STRING_TO_CHAR)
ELEKTRA_TAG_DEFINITIONS (kdb_octet_t, Octet, KDB_TYPE_OCTET, KDB_OCTET_TO_STRING, KDB_STRING_TO_OCTET)
ELEKTRA_TAG_DEFINITIONS (kdb_short_t, Short, KDB_TYPE_SHORT, KDB_SHORT_TO_STRING, KDB_STRING_TO_SHORT)
ELEKTRA_TAG_DEFINITIONS (kdb_unsigned_short_t, UnsignedShort, KDB_TYPE_UNSIGNED_SHORT, KDB_UNSIGNED_SHORT_TO_STRING,
			 KDB_STRING_TO_UNSIGNED_SHORT)
ELEKTRA_TAG_DEFINITIONS (kdb_long_t, Long, KDB_TYPE_LONG, KDB_LONG_TO_STRING, KDB_STRING_TO_LONG)
ELEKTRA_TAG_DEFINITIONS (kdb_unsigned_long_t, UnsignedLong, KDB_TYPE_UNSIGNED_LONG, KDB_UNSIGNED_LONG_TO_STRING,
			 KDB_STRING_TO_UNSIGNED_LONG)
ELEKTRA_TAG_DEFINITIONS (kdb_long_long_t, LongLong, KDB_TYPE_LONG_LONG, KDB_LONG_LONG_TO_STRING, KDB_STRING_TO_LONG_LONG)
ELEKTRA_TAG_DEFINITIONS (kdb_unsigned_long_long_t, UnsignedLongLong, KDB_TYPE_UNSIGNED_LONG_LONG, KDB_UNSIGNED_LONG_LONG_TO_STRING,
			 KDB_STRING_TO_UNSIGNED_LONG_LONG)
ELEKTRA_TAG_DEFINITIONS (kdb_float_t, Float, KDB_TYPE_FLOAT, KDB_FLOAT_TO_STRING, KDB_STRING_TO_FLOAT)
ELEKTRA_TAG_DEFINITIONS (kdb_double_t, Double, KDB_TYPE_DOUBLE, KDB_DOUBLE_TO_STRING, KDB_STRING_TO_DOUBLE)
ELEKTRA_TAG_DEFINITIONS (kdb_long_double_t, LongDouble, KDB_TYPE_LONG_DOUBLE, KDB_LONG_DOUBLE_TO_STRING, KDB_STRING_TO_LONG_DOUBLE)

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
	Key * const key = generateLookupKey (elektra, name);

	KeySet * arrayKeys = elektraArrayGet (key, elektra->config);
	size_t size = (size_t) ksGetSize (arrayKeys);
	ksDel (arrayKeys);

	return size;
}

const char * elektraGetValue (Elektra * elektra, const char * name, KDBType type)
{
	Key * const key = generateLookupKey (elektra, name);

	return getKeyValue (elektra, key, type);
}

const char * elektraGetArrayElementValue (Elektra * elektra, const char * name, size_t index, KDBType type)
{
	Key * const key = generateArrayLookupKey (elektra, name, index);

	return getKeyValue (elektra, key, type);
}

void elektraSetValue (Elektra * elektra, const char * name, const char * value, KDBType type, ElektraError ** error)
{
	Key * const key = keyDup (generateLookupKey (elektra, name));
	setKeyValue (elektra, key, type, value, error);
}

void elektraSetArrayElementValue (Elektra * elektra, const char * name, size_t index, const char * value, KDBType type,
				  ElektraError ** error)
{
	Key * const key = keyDup (generateArrayLookupKey (elektra, name, index));
	setKeyValue (elektra, key, type, value, error);
}

#define ELEKTRA_SET_VALUE(TO_STRING, KDB_TYPE, elektra, keyname, value, error)                                                             \
	elektraSetValue (elektra, keyname, TO_STRING (value), KDB_TYPE, error)

#define ELEKTRA_GET_VALUE(FROM_STRING, KDB_TYPE, elektra, keyname) FROM_STRING (elektraGetValue (elektra, keyname, KDB_TYPE))


void elektraSetString (Elektra * elektra, const char * keyName, const char * value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_STRING_TO_STRING, "string", elektra, keyName, value, error);
}


void elektraSetBoolean (Elektra * elektra, const char * keyname, kdb_boolean_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_BOOLEAN_TO_STRING, "boolean", elektra, keyname, value, error);
}


void elektraSetChar (Elektra * elektra, const char * keyname, kdb_char_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_CHAR_TO_STRING, "char", elektra, keyname, value, error);
}


void elektraSetOctet (Elektra * elektra, const char * keyname, kdb_octet_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_OCTET_TO_STRING, "octet", elektra, keyname, value, error);
}


void elektraSetShort (Elektra * elektra, const char * keyname, kdb_short_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_SHORT_TO_STRING, "short", elektra, keyname, value, error);
}


void elektraSetUnsignedShort (Elektra * elektra, const char * keyname, kdb_unsigned_short_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_UNSIGNED_SHORT_TO_STRING, "unsigned_short", elektra, keyname, value, error);
}


void elektraSetLong (Elektra * elektra, const char * keyname, kdb_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_LONG_TO_STRING, "long", elektra, keyname, value, error);
}


void elektraSetUnsignedLong (Elektra * elektra, const char * keyname, kdb_unsigned_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_UNSIGNED_LONG_TO_STRING, "unsigned_long", elektra, keyname, value, error);
}


void elektraSetLongLong (Elektra * elektra, const char * keyname, kdb_long_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_LONG_LONG_TO_STRING, "long_long", elektra, keyname, value, error);
}


void elektraSetUnsignedLongLong (Elektra * elektra, const char * keyname, kdb_unsigned_long_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_UNSIGNED_LONG_LONG_TO_STRING, "unsigned_long_long", elektra, keyname, value, error);
}


void elektraSetFloat (Elektra * elektra, const char * keyname, kdb_float_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_FLOAT_TO_STRING, "float", elektra, keyname, value, error);
}


void elektraSetDouble (Elektra * elektra, const char * keyname, kdb_double_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_DOUBLE_TO_STRING, "double", elektra, keyname, value, error);
}


void elektraSetLongDouble (Elektra * elektra, const char * keyname, kdb_long_double_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_LONG_DOUBLE_TO_STRING, "long_double", elektra, keyname, value, error);
}

void elektraSetEnum (Elektra * elektra, char * keyName, int value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (KDB_ENUM_TO_STRING, KDB_TYPE_ENUM, elektra, keyName, value, error);
}


const char * elektraGetString (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_STRING, "string", elektra, keyname);
}


kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_BOOLEAN, "boolean", elektra, keyname);
}


kdb_char_t elektraGetChar (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_CHAR, "char", elektra, keyname);
}


kdb_octet_t elektraGetOctet (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_OCTET, "octet", elektra, keyname);
}


kdb_short_t elektraGetShort (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_SHORT, "short", elektra, keyname);
}


kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_UNSIGNED_SHORT, "unsigned_short", elektra, keyname);
}


kdb_long_t elektraGetLong (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_LONG, "long", elektra, keyname);
}


kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_UNSIGNED_LONG, "unsigned_long", elektra, keyname);
}


kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_LONG_LONG, "long_long", elektra, keyname);
}


kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_UNSIGNED_LONG_LONG, "unsigned_long_long", elektra, keyname);
}


kdb_float_t elektraGetFloat (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_FLOAT, "float", elektra, keyname);
}


kdb_double_t elektraGetDouble (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_DOUBLE, "double", elektra, keyname);
}


kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * keyname)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_LONG_DOUBLE, "long_double", elektra, keyname);
}

int elektraGetEnumInt (Elektra * elektra, char * keyName)
{
	return ELEKTRA_GET_VALUE (KDB_STRING_TO_ENUM, KDB_TYPE_ENUM, elektra, keyName);
}


#define ELEKTRA_SET_ARRAY_ELEMENT_VALUE(TO_STRING, KDB_TYPE, elektra, keyname, index, value, error)                                        \
	elektraSetArrayElementValue (elektra, keyname, index, TO_STRING (value), KDB_TYPE, error)

void elektraSetStringArrayElement (Elektra * elektra, const char * keyname, size_t index, const char * value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_STRING, "string", elektra, keyname, index, value, error);
}


void elektraSetBooleanArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_boolean_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_BOOLEAN_TO_STRING, "boolean", elektra, keyname, index, value, error);
}


void elektraSetCharArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_char_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_CHAR_TO_STRING, "char", elektra, keyname, index, value, error);
}


void elektraSetOctetArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_octet_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_OCTET_TO_STRING, "octet", elektra, keyname, index, value, error);
}


void elektraSetShortArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_short_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_SHORT_TO_STRING, "short", elektra, keyname, index, value, error);
}


void elektraSetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_short_t value,
					  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_UNSIGNED_SHORT_TO_STRING, "unsigned_short", elektra, keyname, index, value, error);
}


void elektraSetLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_LONG_TO_STRING, "long", elektra, keyname, index, value, error);
}


void elektraSetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_long_t value,
					 ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_UNSIGNED_LONG_TO_STRING, "unsigned_long", elektra, keyname, index, value, error);
}


void elektraSetLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_LONG_LONG_TO_STRING, "long_long", elektra, keyname, index, value, error);
}


void elektraSetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_long_long_t value,
					     ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_UNSIGNED_LONG_LONG_TO_STRING, "unsigned_long_long", elektra, keyname, index, value, error);
}


void elektraSetFloatArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_float_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_FLOAT_TO_STRING, "float", elektra, keyname, index, value, error);
}


void elektraSetDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_double_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_DOUBLE_TO_STRING, "double", elektra, keyname, index, value, error);
}


void elektraSetLongDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_double_t value,
				       ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_LONG_DOUBLE_TO_STRING, "long_double", elektra, keyname, index, value, error);
}

void elektraSetEnumArrayElement (Elektra * elektra, char * keyName, size_t index, int value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (KDB_ENUM_TO_STRING, KDB_TYPE_ENUM, elektra, keyName, index, value, error);
}


#define ELEKTRA_GET_ARRAY_ELEMENT_VALUE(FROM_STRING, KDB_TYPE, elektra, keyname, index)                                                    \
	FROM_STRING (elektraGetArrayElementValue (elektra, keyname, index, KDB_TYPE))


const char * elektraGetStringArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_STRING, "string", elektra, keyname, index);
}


kdb_boolean_t elektraGetBooleanArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_BOOLEAN, "boolean", elektra, keyname, index);
}


kdb_char_t elektraGetCharArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_CHAR, "char", elektra, keyname, index);
}


kdb_octet_t elektraGetOctetArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_OCTET, "octet", elektra, keyname, index);
}


kdb_short_t elektraGetShortArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_SHORT, "short", elektra, keyname, index);
}


kdb_unsigned_short_t elektraGetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_UNSIGNED_SHORT, "unsigned_short", elektra, keyname, index);
}


kdb_long_t elektraGetLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_LONG, "long", elektra, keyname, index);
}


kdb_unsigned_long_t elektraGetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_UNSIGNED_LONG, "unsigned_long", elektra, keyname, index);
}


kdb_long_long_t elektraGetLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_LONG_LONG, "long_long", elektra, keyname, index);
}


kdb_unsigned_long_long_t elektraGetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_UNSIGNED_LONG_LONG, "unsigned_long_long", elektra, keyname, index);
}


kdb_float_t elektraGetFloatArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_FLOAT, "float", elektra, keyname, index);
}


kdb_double_t elektraGetDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_DOUBLE, "double", elektra, keyname, index);
}


kdb_long_double_t elektraGetLongDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_LONG_DOUBLE, "long_double", elektra, keyname, index);
}

int elektraGetEnumArrayElementInt (Elektra * elektra, char * keyName, int index)
{
	return ELEKTRA_GET_ARRAY_ELEMENT_VALUE (KDB_STRING_TO_ENUM, KDB_TYPE_ENUM, elektra, keyName, index);
}

/**
 * @}
 */

// Private functions

static void saveKey (Elektra * elektra, Key * key, ElektraError ** error)
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

static void checkType (Key * key, KDBType type)
{
	if (strcmp (keyString (keyGetMeta (key, "type")), type) != 0)
	{
		ELEKTRA_LOG_DEBUG ("Wrong type. Should be: %s\n", type);
		exit (EXIT_FAILURE);
	}
}

static Key * generateLookupKey (Elektra * elektra, const char * name)
{
	Key * const lookupKey = elektra->lookupKey;

	keySetName (lookupKey, keyName (elektra->parentKey));
	keyAddName (lookupKey, name);

	return lookupKey;
}

static Key * generateArrayLookupKey (Elektra * elektra, const char * name, size_t index)
{
	Key * const lookupKey = generateLookupKey (elektra, name);

	char arrayPart[ELEKTRA_MAX_ARRAY_SIZE];
	elektraWriteArrayNumber (arrayPart, index);
	keyAddName (lookupKey, arrayPart);

	return lookupKey;
}

// Set values

static void setKeyValue (Elektra * elektra, Key * key, KDBType type, const char * value, ElektraError ** error)
{
	keySetMeta (key, "type", type);
	keySetString (key, value);

	saveKey (elektra, key, error);
}

// Get values

static const char * getKeyValue (Elektra * elektra, Key * key, KDBType type)
{
	Key * const resultKey = ksLookup (elektra->config, key, 0);
	if (resultKey == NULL)
	{
		ELEKTRA_LOG_DEBUG ("Key not found: %s\n", keyName (key));
		exit (EXIT_FAILURE);
	}

	checkType (resultKey, type);

	return keyString (resultKey);
}

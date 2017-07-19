/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra.h"
#include "elektra_error_private.h"
#include "elektra_private.h"
//#include "kdberrors.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <memory.h>
#include <stdlib.h>

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

static Key * generateLookupKey (Elektra * elektra, const char * name);

/**
 * \defgroup highlevel High-level API
 * @{
 */

ELEKTRA_DEFINITIONS (const char *, String, KDB_TYPE_STRING, KDB_STRING_TO_STRING, KDB_STRING_TO_STRING)
ELEKTRA_DEFINITIONS (kdb_boolean_t, Boolean, KDB_TYPE_BOOLEAN, KDB_BOOLEAN_TO_STRING, KDB_STRING_TO_BOOLEAN)
ELEKTRA_DEFINITIONS (kdb_char_t, Char, KDB_TYPE_CHAR, KDB_CHAR_TO_STRING, KDB_STRING_TO_CHAR)
ELEKTRA_DEFINITIONS (kdb_octet_t, Octet, KDB_TYPE_OCTET, KDB_OCTET_TO_STRING, KDB_STRING_TO_OCTET)
ELEKTRA_DEFINITIONS (kdb_short_t, Short, KDB_TYPE_SHORT, KDB_SHORT_TO_STRING, KDB_STRING_TO_SHORT)
ELEKTRA_DEFINITIONS (kdb_unsigned_short_t, UnsignedShort, KDB_TYPE_UNSIGNED_SHORT, KDB_UNSIGNED_SHORT_TO_STRING, KDB_STRING_TO_UNSIGNED_SHORT)
ELEKTRA_DEFINITIONS (kdb_long_t, Long, KDB_TYPE_LONG, KDB_LONG_TO_STRING, KDB_STRING_TO_LONG)
ELEKTRA_DEFINITIONS (kdb_unsigned_long_t, UnsignedLong, KDB_TYPE_UNSIGNED_LONG, KDB_UNSIGNED_LONG_TO_STRING, KDB_STRING_TO_UNSIGNED_LONG)
ELEKTRA_DEFINITIONS (kdb_long_long_t, LongLong, KDB_TYPE_LONG_LONG, KDB_LONG_LONG_TO_STRING, KDB_STRING_TO_LONG_LONG)
ELEKTRA_DEFINITIONS (kdb_unsigned_long_long_t, UnsignedLongLong, KDB_TYPE_UNSIGNED_LONG_LONG, KDB_UNSIGNED_LONG_LONG_TO_STRING, KDB_STRING_TO_UNSIGNED_LONG_LONG)
ELEKTRA_DEFINITIONS (kdb_float_t, Float, KDB_TYPE_FLOAT, KDB_FLOAT_TO_STRING, KDB_STRING_TO_FLOAT)
ELEKTRA_DEFINITIONS (kdb_double_t, Double, KDB_TYPE_DOUBLE, KDB_DOUBLE_TO_STRING, KDB_STRING_TO_DOUBLE)
ELEKTRA_DEFINITIONS (kdb_long_double_t, LongDouble, KDB_TYPE_LONG_DOUBLE, KDB_LONG_DOUBLE_TO_STRING, KDB_STRING_TO_LONG_DOUBLE)

/**
 * Initializes a new Elektra instance.
 * @param application The parent key for your application.
 * @param defaults A KeySet containing default values. Passing NULL means "no default values".
 * @return An Elektra instance initialized with the application.
 */
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
		ksAppend (config, defaults);
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

/**
 * Releases all ressources used by the given elektra instance. The elektra instance must not be used anymore after calling this.
 * @param elektra An Elektra instance.
 */
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
	size_t size = (size_t)ksGetSize (arrayKeys);
	ksDel (arrayKeys);

	return size;
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
	if (strcmp (keyString (keyGetMeta (key, "type")), type))
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

void setValueAsString (Elektra * elektra, const char * name, const char * value, KDBType type, ElektraError ** error)
{
	Key * const key = keyDup (generateLookupKey (elektra, name));
	setKeyValue (elektra, key, type, value, error);
}

void setArrayElementValueAsString (Elektra * elektra, const char * name, const char * value, KDBType type, size_t index,
					  ElektraError ** error)
{
	Key * const key = keyDup (generateArrayLookupKey (elektra, name, index));
	setKeyValue (elektra, key, type, value, error);
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

const char * getValueAsString (Elektra * elektra, const char * name, KDBType type)
{
	Key * const key = generateLookupKey (elektra, name);

	return getKeyValue (elektra, key, type);
}

const char * getArrayElementValueAsString (Elektra * elektra, const char * name, KDBType type, size_t index)
{
	Key * const key = generateArrayLookupKey (elektra, name, index);

	return getKeyValue (elektra, key, type);
}

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

/**
 * Helper function for code generation.
 * 
 * Finds a Key from its relative name. Also checks type metadata,
 * if type metadata is enforces for the given Elektra instance.
 * 
 * @param elektra The Elektra instance to use.
 * @param name    The relative name of the key.
 * @param type    The expected type metadata value.
 * @return the Key referenced by @p name
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

/**
 * Get the raw value of a key.
 * 
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the key.
 * @return the raw value of the specified key
 */
const char * elektraGetValue (Elektra * elektra, const char * name)
{
	return keyString (elektraFindKey (elektra, name, NULL));
}

/**
 * Set the raw value of a key.
 * 
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the key.
 * @param value   The raw value to set.
 * @param type    The type to set in the metadata of the key.
 * @param error   Pointer to an ElektraError. Will be set in case saving fails.
 */
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
 * Gets a string value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the string stored at the given key
 */
const char * elektraGetString (Elektra * elektra, const char * keyname)
{
	const char * result;
	ELEKTRA_GET_VALUE (elektraKeyToString, KDB_TYPE_STRING, elektra, keyname, result);
	return result;
}

/**
 * Gets a boolean value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the boolean stored at the given key
 */
kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * keyname)
{
	kdb_boolean_t result;
	ELEKTRA_GET_VALUE (elektraKeyToBoolean, KDB_TYPE_BOOLEAN, elektra, keyname, result);
	return result;
}

/**
 * Gets a char value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the char stored at the given key
 */
kdb_char_t elektraGetChar (Elektra * elektra, const char * keyname)
{
	kdb_char_t result;
	ELEKTRA_GET_VALUE (elektraKeyToChar, KDB_TYPE_CHAR, elektra, keyname, result);
	return result;
}

/**
 * Gets a octet value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the octet stored at the given key
 */
kdb_octet_t elektraGetOctet (Elektra * elektra, const char * keyname)
{
	kdb_octet_t result;
	ELEKTRA_GET_VALUE (elektraKeyToOctet, KDB_TYPE_OCTET, elektra, keyname, result);
	return result;
}

/**
 * Gets a short value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the short stored at the given key
 */
kdb_short_t elektraGetShort (Elektra * elektra, const char * keyname)
{
	kdb_short_t result;
	ELEKTRA_GET_VALUE (elektraKeyToShort, KDB_TYPE_SHORT, elektra, keyname, result);
	return result;
}

/**
 * Gets a unsigned short value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the unsigned short stored at the given key
 */
kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * keyname)
{
	kdb_unsigned_short_t result;
	ELEKTRA_GET_VALUE (elektraKeyToUnsignedShort, KDB_TYPE_UNSIGNED_SHORT, elektra, keyname, result);
	return result;
}

/**
 * Gets a long value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the long stored at the given key
 */
kdb_long_t elektraGetLong (Elektra * elektra, const char * keyname)
{
	kdb_long_t result;
	ELEKTRA_GET_VALUE (elektraKeyToLong, KDB_TYPE_LONG, elektra, keyname, result);
	return result;
}

/**
 * Gets a unsigned long value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the unsigned long stored at the given key
 */
kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * keyname)
{
	kdb_unsigned_long_t result;
	ELEKTRA_GET_VALUE (elektraKeyToUnsignedLong, KDB_TYPE_UNSIGNED_LONG, elektra, keyname, result);
	return result;
}

/**
 * Gets a long long value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the long long stored at the given key
 */
kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * keyname)
{
	kdb_long_long_t result;
	ELEKTRA_GET_VALUE (elektraKeyToLongLong, KDB_TYPE_LONG_LONG, elektra, keyname, result);
	return result;
}

/**
 * Gets a long long value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the unsigned long long stored at the given key
 */
kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * keyname)
{
	kdb_unsigned_long_long_t result;
	ELEKTRA_GET_VALUE (elektraKeyToUnsignedLongLong, KDB_TYPE_UNSIGNED_LONG_LONG, elektra, keyname, result);
	return result;
}

/**
 * Gets a float value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the float stored at the given key
 */
kdb_float_t elektraGetFloat (Elektra * elektra, const char * keyname)
{
	kdb_float_t result;
	ELEKTRA_GET_VALUE (elektraKeyToFloat, KDB_TYPE_FLOAT, elektra, keyname, result);
	return result;
}

/**
 * Gets a double value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the double stored at the given key
 */
kdb_double_t elektraGetDouble (Elektra * elektra, const char * keyname)
{
	kdb_double_t result;
	ELEKTRA_GET_VALUE (elektraKeyToDouble, KDB_TYPE_DOUBLE, elektra, keyname, result);
	return result;
}

/**
 * Gets a long double value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the long double stored at the given key
 */
kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * keyname)
{
	kdb_long_double_t result;
	ELEKTRA_GET_VALUE (elektraKeyToLongDouble, KDB_TYPE_LONG_DOUBLE, elektra, keyname, result);
	return result;
}

/**
 * Gets the int value of a stored enum value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the int value of the enum stored at the given key
 */
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
 * Sets a string value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new string value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetString (Elektra * elektra, const char * keyname, const char * value, ElektraError ** error)
{
	elektraSetValue (elektra, keyName, value, KDB_TYPE_STRING, error);
}

/**
 * Sets a boolean value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new boolean value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetBoolean (Elektra * elektra, const char * keyname, kdb_boolean_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraBooleanToString, KDB_TYPE_BOOLEAN, elektra, keyname, value, error);
}

/**
 * Sets a char value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new char value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetChar (Elektra * elektra, const char * keyname, kdb_char_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraCharToString, KDB_TYPE_CHAR, elektra, keyname, value, error);
}

/**
 * Sets a octet value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new octet value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetOctet (Elektra * elektra, const char * keyname, kdb_octet_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraOctetToString, KDB_TYPE_OCTET, elektra, keyname, value, error);
}

/**
 * Sets a short value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new short value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetShort (Elektra * elektra, const char * keyname, kdb_short_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraShortToString, KDB_TYPE_SHORT, elektra, keyname, value, error);
}

/**
 * Sets a unsigned short value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new unsigned short value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetUnsignedShort (Elektra * elektra, const char * keyname, kdb_unsigned_short_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraUnsignedShortToString, KDB_TYPE_UNSIGNED_SHORT, elektra, keyname, value, error);
}

/**
 * Sets a long value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new long value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetLong (Elektra * elektra, const char * keyname, kdb_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraLongToString, KDB_TYPE_LONG, elektra, keyname, value, error);
}

/**
 * Sets a unsigned long value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new unsigned long value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetUnsignedLong (Elektra * elektra, const char * keyname, kdb_unsigned_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraUnsignedLongToString, KDB_TYPE_UNSIGNED_LONG, elektra, keyname, value, error);
}

/**
 * Sets a long long value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new long long value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetLongLong (Elektra * elektra, const char * keyname, kdb_long_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraLongLongToString, KDB_TYPE_LONG_LONG, elektra, keyname, value, error);
}

/**
 * Sets a unsigned long long value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new unsigned long long value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetUnsignedLongLong (Elektra * elektra, const char * keyname, kdb_unsigned_long_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraUnsignedLongLongToString, KDB_TYPE_UNSIGNED_LONG_LONG, elektra, keyname, value, error);
}

/**
 * Sets a float value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new float value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetFloat (Elektra * elektra, const char * keyname, kdb_float_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraFloatToString, KDB_TYPE_FLOAT, elektra, keyname, value, error);
}

/**
 * Sets a double value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new double value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetDouble (Elektra * elektra, const char * keyname, kdb_double_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraDoubleToString, KDB_TYPE_DOUBLE, elektra, keyname, value, error);
}

/**
 * Sets a long double value.
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new long double value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetLongDouble (Elektra * elektra, const char * keyname, kdb_long_double_t value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraLongDoubleToString, KDB_TYPE_LONG_DOUBLE, elektra, keyname, value, error);
}

/**
 * Sets an enum value. The corresponding int value will be
 * stored with the type metadata set to "enum".
 * 
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name to write to.
 * @param value   The new value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetEnumInt (Elektra * elektra, char * keyName, int value, ElektraError ** error)
{
	ELEKTRA_SET_VALUE (elektraLongToString, KDB_TYPE_ENUM, elektra, keyName, value, error);
}

/**
 * @}
 */

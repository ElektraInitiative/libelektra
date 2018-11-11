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
 * \addtogroup highlevel High-level API
 * @{
 */

/**
 * Gets the size of an array.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the array.
 * @return the size of the array
 */
size_t elektraArraySize (Elektra * elektra, const char * name)
{
	elektraSetLookupKey (elektra, name);
	KeySet * arrayKeys = elektraArrayGet (elektra->lookupKey, elektra->config);
	size_t size = (size_t) ksGetSize (arrayKeys);
	ksDel (arrayKeys);

	return size;
}

/**
 * Helper function for code generation.
 *
 * Finds an array element Key from its relative name and index.
 * Also checks type metadata, if type metadata is enforces for
 * the given Elektra instance.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The relative name of the array.
 * @param index   The index of the array element.
 * @param type    The expected type metadata value.
 * @return the Key referenced by @p name or NULL, if a fatal error occurs and the fatal error handler returns to this function
 */
Key * elektraFindArrayElementKey (Elektra * elektra, const char * name, size_t index, KDBType type)
{
	elektraSetArrayLookupKey (elektra, name, index);
	Key * const resultKey = ksLookup (elektra->config, elektra->lookupKey, 0);
	if (resultKey == NULL)
	{
		elektraFatalError (elektra, elektraErrorKeyNotFound (keyName (elektra->lookupKey), NULL));
		return NULL;
	}

	if (!elektra->enforceType && type != NULL)
	{
		const char * actualType = keyString (keyGetMeta (resultKey, "type"));
		if (strcmp (actualType, type) != 0)
		{
			elektraFatalError (elektra, elektraErrorWrongType (keyName (elektra->lookupKey), actualType, type, NULL));
			return NULL;
		}
	}

	return resultKey;
}

/**
 * Reads the type metadata of a given array element.
 *
 * @param elektra An Elektra instance.
 * @param name    The name of the array.
 * @param index   The index of the array element whose type information shall be read.
 * @return the KDBType of the key
 */
KDBType elektraGetArrayElementType (Elektra * elektra, const char * keyname, size_t index)
{
	elektraSetLookupKey (elektra, keyname);
	const Key * key = elektraFindArrayElementKey (elektra, keyname, index, NULL);
	const Key * metaKey = keyGetMeta (key, "type");
	return metaKey == NULL ? NULL : keyString (metaKey);
}

/**
 * Get the raw value of an array element key.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the array.
 * @param index   The index of the array element.
 * @return the raw value of the specified key, or NULL if the key was not found
 */
const char * elektraGetArrayElementValue (Elektra * elektra, const char * name, size_t index)
{
	elektraSetArrayLookupKey (elektra, name, index);
	Key * const resultKey = ksLookup (elektra->config, elektra->lookupKey, 0);
	return resultKey == NULL ? NULL : keyString (resultKey);
}

/**
 * Set the raw value of an array element key.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the array.
 * @param index   The index of the array element.
 * @param value   The raw value to set.
 * @param type    The type to set in the metadata of the (array element) key.
 * @param error   Pointer to an ElektraError. Will be set in case saving fails.
 */
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
		return 0;                                                                                                                  \
	}

/**
 * Gets a string value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the string stored at the given array element
 */
const char * elektraGetStringArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	const char * result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToString, KDB_TYPE_STRING, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a boolean value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the boolean stored at the given array element
 */
kdb_boolean_t elektraGetBooleanArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_boolean_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToBoolean, KDB_TYPE_BOOLEAN, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a char value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the char stored at the given array element
 */
kdb_char_t elektraGetCharArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_char_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToChar, KDB_TYPE_CHAR, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a octet value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the octet stored at the given array element
 */
kdb_octet_t elektraGetOctetArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_octet_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToOctet, KDB_TYPE_OCTET, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a short value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the short stored at the given array element
 */
kdb_short_t elektraGetShortArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_short_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToShort, KDB_TYPE_SHORT, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a unsigned short value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the unsigned short stored at the given array element
 */
kdb_unsigned_short_t elektraGetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_unsigned_short_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToUnsignedShort, KDB_TYPE_UNSIGNED_SHORT, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a long value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the long stored at the given array element
 */
kdb_long_t elektraGetLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_long_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToLong, KDB_TYPE_LONG, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a unsigned long value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the unsigned long stored at the given array element
 */
kdb_unsigned_long_t elektraGetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_unsigned_long_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToUnsignedLong, KDB_TYPE_UNSIGNED_LONG, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a long long value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the long long stored at the given array element
 */
kdb_long_long_t elektraGetLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_long_long_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToLongLong, KDB_TYPE_LONG_LONG, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a unsigned long long value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the unsigned long long stored at the given array element
 */
kdb_unsigned_long_long_t elektraGetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_unsigned_long_long_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToUnsignedLongLong, KDB_TYPE_UNSIGNED_LONG_LONG, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a float value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the float stored at the given array element
 */
kdb_float_t elektraGetFloatArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_float_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToFloat, KDB_TYPE_FLOAT, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a double value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the double stored at the given array element
 */
kdb_double_t elektraGetDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_double_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToDouble, KDB_TYPE_DOUBLE, elektra, keyname, index, result);
	return result;
}

/**
 * Gets a long double value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the long double stored at the given array element
 */
kdb_long_double_t elektraGetLongDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index)
{
	kdb_long_double_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToLongDouble, KDB_TYPE_LONG_DOUBLE, elektra, keyname, index, result);
	return result;
}

/**
 * Gets the int value of a stored enum array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the int value of the enum stored at the given array element
 */
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

/**
 * Sets a string value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new string value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetStringArrayElement (Elektra * elektra, const char * keyname, size_t index, const char * value, ElektraError ** error)
{
	elektraSetArrayElementValue (elektra, keyname, index, value, KDB_TYPE_STRING, error);
}

/**
 * Sets a boolean value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new boolean value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetBooleanArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_boolean_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraBooleanToString, KDB_TYPE_BOOLEAN, elektra, keyname, index, value, error);
}

/**
 * Sets a char value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new char value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetCharArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_char_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraCharToString, KDB_TYPE_CHAR, elektra, keyname, index, value, error);
}

/**
 * Sets a octet value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new octet value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetOctetArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_octet_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraOctetToString, KDB_TYPE_OCTET, elektra, keyname, index, value, error);
}

/**
 * Sets a short value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new short value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetShortArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_short_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraShortToString, KDB_TYPE_SHORT, elektra, keyname, index, value, error);
}

/**
 * Sets a unsigned short value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new unsigned short value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_short_t value,
					  ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraUnsignedShortToString, KDB_TYPE_UNSIGNED_SHORT, elektra, keyname, index, value, error);
}

/**
 * Sets a long value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new long value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraLongToString, KDB_TYPE_LONG, elektra, keyname, index, value, error);
}

/**
 * Sets a unsigned long value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new unsigned long value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_long_t value,
					 ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraUnsignedLongToString, KDB_TYPE_UNSIGNED_LONG, elektra, keyname, index, value, error);
}

/**
 * Sets a long long value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new long long value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_long_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraLongLongToString, KDB_TYPE_LONG_LONG, elektra, keyname, index, value, error);
}

/**
 * Sets a unsigned long long value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new unsigned long long value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_long_long_t value,
					     ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraUnsignedLongLongToString, KDB_TYPE_UNSIGNED_LONG_LONG, elektra, keyname, index, value,
					 error);
}

/**
 * Sets a float value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new float value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetFloatArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_float_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraFloatToString, KDB_TYPE_FLOAT, elektra, keyname, index, value, error);
}

/**
 * Sets a double value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new double value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_double_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraDoubleToString, KDB_TYPE_DOUBLE, elektra, keyname, index, value, error);
}

/**
 * Sets a long double value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new long double value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetLongDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_double_t value,
				       ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraLongDoubleToString, KDB_TYPE_LONG_DOUBLE, elektra, keyname, index, value, error);
}

/**
 * Sets an enum value array element. The corresponding int value will be
 * stored with the type metadata set to "enum".
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetEnumIntArrayElement (Elektra * elektra, char * keyName, size_t index, int value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraLongToString, KDB_TYPE_ENUM, elektra, keyName, index, value, error);
}

/**
 * @}
 */

// Private functions
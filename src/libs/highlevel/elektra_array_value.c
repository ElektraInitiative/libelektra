/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra/elektra.h"
#include "elektra/conversion.h"
#include "elektra/kdbease.h"
#include "elektra/kdbhelper.h"
#include "elektra/kdbprivate.h"

#include <stdlib.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

#define CHECK_ERROR(elektra, error)                                                                                                        \
	if (error == NULL)                                                                                                                 \
	{                                                                                                                                  \
		elektraFatalError (elektra, elektraErrorNullError (__func__));                                                             \
		return;                                                                                                                    \
	}

/**
 * Sets the size of an array
 *
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the array.
 * @param size    The new size of the array.
 */
static void elektraArraySetSize (Elektra * elektra, const char * name, kdb_long_long_t size, ElektraError ** error)
{
	elektraSetLookupKey (elektra, name);
	Key * arrayParent = keyDup (elektra->lookupKey, KEY_CP_NAME);

	char sizeString[ELEKTRA_MAX_ARRAY_SIZE];
	elektraWriteArrayNumber (sizeString, size - 1);
	keySetMeta (arrayParent, "array", sizeString);
	keySetString (arrayParent, "");

	elektraSaveKey (elektra, arrayParent, error);
}

/**
 * \addtogroup highlevel High-level API
 * @{
 */

/**
 * Gets the size of an array.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the array.
 * @return the size of the array, 0 is returned if the array is empty or doesn't exist
 */
kdb_long_long_t elektraArraySize (Elektra * elektra, const char * name)
{
	elektraSetLookupKey (elektra, name);
	Key * const arrayParent = ksLookup (elektra->config, elektra->lookupKey, 0);
	if (arrayParent == NULL)
	{
		return 0;
	}

	const Key * const metaKey = keyGetMeta (arrayParent, "array");
	if (metaKey == NULL)
	{
		return 0;
	}

	const char * sizeString = keyString (metaKey);
	int digitStart = elektraArrayValidateBaseNameString (sizeString);
	if (digitStart <= 0)
	{
		return 0;
	}

	kdb_long_long_t size = strtoll (&sizeString[digitStart], NULL, 10) + 1;

	return size;
}

/**
 * Helper function for code generation.
 *
 * Finds an array element Key from its relative name and index.
 * Also checks type metadata, if @p type is not NULL.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The relative name of the array.
 * @param index   The index of the array element.
 * @param type    The expected type metadata value.
 * @return the Key referenced by @p name or NULL, if a fatal error occurs and the fatal error handler returns to this function
 *   The returned pointer remains valid until the KeySet inside @p elektra is modified. Calls to elektraSet*() functions may
 *   cause such modifications. In any case, it becomes invalid when elektraClose() is called on @p elektra.
 */
Key * elektraFindArrayElementKey (Elektra * elektra, const char * name, kdb_long_long_t index, KDBType type)
{
	elektraSetArrayLookupKey (elektra, name, index);
	Key * const resultKey = ksLookup (elektra->config, elektra->lookupKey, 0);
	if (resultKey == NULL)
	{
		elektraFatalError (elektra, elektraErrorKeyNotFound (keyName (elektra->lookupKey)));
		return NULL;
	}

	if (type != NULL)
	{
		const char * actualType = keyString (keyGetMeta (resultKey, "type"));
		if (strcmp (actualType, type) != 0)
		{
			elektraFatalError (elektra, elektraErrorWrongType (keyName (elektra->lookupKey), type, actualType));
			return NULL;
		}
	}

	return resultKey;
}

/**
 * Resolves the reference stored in a key.
 * 1. Get the raw string value.
 * 2. Resolve that reference.
 * 3. Return resulting keyname relative to the parent key of the given Elektra instance.
 *
 * IMPORTANT: this method DOES NOT check the type metadata of the key, it is only intended
 * to be used by the code-generation API.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the array.
 * @param index   The index of the array element.
 * @return the resolved version of the reference stored in the specified key (relative to the parent key of @p elektra)
 * or NULL, if the key was not found, or the reference resolves two a key not below the parent key. The empty string is
 * returned, if the value was the empty string (no resolution is attempted).
 *   The returned pointer becomes invalid when this function is called again (even with the same arguments). It is also
 *   invalidated when elektraFindReference() or elektraClose() are called on @p elektra.
 */
const char * elektraFindReferenceArrayElement (Elektra * elektra, const char * name, kdb_long_long_t index)
{
	elektraSetArrayLookupKey (elektra, name, index);
	Key * const resultKey = ksLookup (elektra->config, elektra->lookupKey, 0);
	if (resultKey == NULL)
	{
		return NULL;
	}

	const char * reference = keyString (resultKey);

	if (strlen (reference) == 0)
	{
		return "";
	}

	if (elektra->resolvedReference != NULL)
	{
		elektraFree (elektra->resolvedReference);
		elektra->resolvedReference = NULL;
	}

	elektra->resolvedReference = elektraResolveReference (reference, elektra->lookupKey, elektra->parentKey);

	size_t len = strlen (elektra->resolvedReference);
	if (len < elektra->parentKeyLength ||
	    strncmp (keyName (elektra->parentKey), elektra->resolvedReference, elektra->parentKeyLength) != 0)
	{
		return NULL;
	}

	return &elektra->resolvedReference[elektra->parentKeyLength];
}

/**
 * Reads the type metadata of a given array element.
 *
 * @param elektra An Elektra instance.
 * @param name    The name of the array.
 * @param index   The index of the array element whose type information shall be read.
 * @return the KDBType of the key
 */
KDBType elektraGetArrayElementType (Elektra * elektra, const char * keyname, kdb_long_long_t index)
{
	elektraSetArrayLookupKey (elektra, keyname, index);
	const Key * key = elektraFindArrayElementKey (elektra, keyname, index, NULL);
	const Key * metaKey = keyGetMeta (key, "type");
	return metaKey == NULL ? NULL : keyString (metaKey);
}

/**
 * Get the raw string value of an array element key.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the array.
 * @param index   The index of the array element.
 * @return the raw value of the specified key, or NULL if the key was not found
 *   The returned pointer remains valid until the internal state of @p elektra is modified.
 *   Calls to elektraSet*() functions may cause such modifications. In any case, it becomes
 *   invalid when elektraClose() is called on @p elektra.
 */
const char * elektraGetRawStringArrayElement (Elektra * elektra, const char * name, kdb_long_long_t index)
{
	elektraSetArrayLookupKey (elektra, name, index);
	Key * const resultKey = ksLookup (elektra->config, elektra->lookupKey, 0);
	return resultKey == NULL ? NULL : keyString (resultKey);
}

/**
 * Set the raw string value of an array element key.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the array.
 * @param index   The index of the array element.
 * @param value   The raw value to set.
 * @param type    The type to set in the metadata of the (array element) key.
 * @param error   Pointer to an ElektraError. Will be set in case saving fails.
 */
void elektraSetRawStringArrayElement (Elektra * elektra, const char * name, kdb_long_long_t index, const char * value, KDBType type,
				      ElektraError ** error)
{
	CHECK_ERROR (elektra, error);

	if (elektraArraySize (elektra, name) < index)
	{
		elektraArraySetSize (elektra, name, index + 1, error);
		if (*error != NULL)
		{
			return;
		}
	}

	elektraSetArrayLookupKey (elektra, name, index);
	Key * key = ksLookup (elektra->config, elektra->lookupKey, 0);
	if (key == NULL)
	{
		key = keyDup (elektra->lookupKey, KEY_CP_ALL);
	}
	keySetMeta (key, "type", type);
	keySetString (key, value);

	elektraSaveKey (elektra, key, error);
}

#define ELEKTRA_GET_ARRAY_ELEMENT_VALUE(KEY_TO_VALUE, KDB_TYPE, elektra, keyname, index, result)                                           \
	const Key * key = elektraFindArrayElementKey (elektra, keyname, index, KDB_TYPE);                                                  \
	if (key == NULL || !KEY_TO_VALUE (key, &result))                                                                                   \
	{                                                                                                                                  \
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE, keyname, keyString (key)));                        \
		return 0;                                                                                                                  \
	}

/**
 * Gets a string value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the string stored at the given array element
 *   The returned pointer remains valid until the internal state of @p elektra is modified.
 *   Calls to elektraSet*() functions may cause such modifications. In any case, it becomes
 *   invalid when elektraClose() is called on @p elektra.
 */
const char * elektraGetStringArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
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
kdb_boolean_t elektraGetBooleanArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
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
kdb_char_t elektraGetCharArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
{
	kdb_char_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToChar, KDB_TYPE_CHAR, elektra, keyname, index, result);
	return result;
}

/**
 * Gets an octet value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the octet stored at the given array element
 */
kdb_octet_t elektraGetOctetArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
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
kdb_short_t elektraGetShortArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
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
kdb_unsigned_short_t elektraGetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
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
kdb_long_t elektraGetLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
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
kdb_unsigned_long_t elektraGetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
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
kdb_long_long_t elektraGetLongLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
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
kdb_unsigned_long_long_t elektraGetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
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
kdb_float_t elektraGetFloatArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
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
kdb_double_t elektraGetDoubleArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
{
	kdb_double_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToDouble, KDB_TYPE_DOUBLE, elektra, keyname, index, result);
	return result;
}

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

/**
 * Gets a long double value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to look up.
 * @param index   The index of the array element to look up.
 * @return the long double stored at the given array element
 */
kdb_long_double_t elektraGetLongDoubleArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index)
{
	kdb_long_double_t result;
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE (elektraKeyToLongDouble, KDB_TYPE_LONG_DOUBLE, elektra, keyname, index, result);
	return result;
}

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

#define ELEKTRA_SET_ARRAY_ELEMENT_VALUE(VALUE_TO_STRING, KDB_TYPE, elektra, keyname, index, value, error)                                  \
	CHECK_ERROR (elektra, error);                                                                                                      \
	char * string = VALUE_TO_STRING (value);                                                                                           \
	if (string == 0)                                                                                                                   \
	{                                                                                                                                  \
		*error = elektraErrorConversionToString (KDB_TYPE, keyname);                                                               \
		return;                                                                                                                    \
	}                                                                                                                                  \
	elektraSetRawStringArrayElement (elektra, keyname, index, string, KDB_TYPE, error);                                                \
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
void elektraSetStringArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, const char * value,
				   ElektraError ** error)
{
	CHECK_ERROR (elektra, error);
	elektraSetRawStringArrayElement (elektra, keyname, index, value, KDB_TYPE_STRING, error);
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
void elektraSetBooleanArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_boolean_t value,
				    ElektraError ** error)
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
void elektraSetCharArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_char_t value, ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraCharToString, KDB_TYPE_CHAR, elektra, keyname, index, value, error);
}

/**
 * Sets an octet value array element.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the array to write to.
 * @param index   The index of the array element to write to.
 * @param value   The new octet value.
 * @param error   Pass a reference to an ElektraError pointer.
 *                Will only be set in case of an error.
 */
void elektraSetOctetArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_octet_t value, ElektraError ** error)
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
void elektraSetShortArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_short_t value, ElektraError ** error)
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
void elektraSetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_unsigned_short_t value,
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
void elektraSetLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_long_t value, ElektraError ** error)
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
void elektraSetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_unsigned_long_t value,
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
void elektraSetLongLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_long_long_t value,
				     ElektraError ** error)
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
void elektraSetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_unsigned_long_long_t value,
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
void elektraSetFloatArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_float_t value, ElektraError ** error)
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
void elektraSetDoubleArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_double_t value,
				   ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraDoubleToString, KDB_TYPE_DOUBLE, elektra, keyname, index, value, error);
}

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

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
void elektraSetLongDoubleArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_long_double_t value,
				       ElektraError ** error)
{
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE (elektraLongDoubleToString, KDB_TYPE_LONG_DOUBLE, elektra, keyname, index, value, error);
}

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

/**
 * @}
 */

#ifdef __cplusplus
};
#endif

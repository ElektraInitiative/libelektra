/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <elektra/type/conversion.h>
#include <elektra/ease/reference.h>
#include <elektra/highlevel.h>
#include <elektra/type/conversion.h>
#include <internal/kdbprivate.h>
#include <internal/utility/old_helper.h>
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
 * \addtogroup highlevel High-level API
 * @{
 */

/**
 * Helper function for code generation.
 *
 * Finds a Key from its relative name. Also checks type metadata,
 * if @p type is not NULL.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The relative name of the key.
 * @param type    The expected type metadata value.
 * @return the Key referenced by @p name or NULL, if a fatal error occurs and the fatal error handler returns to this function
 *   The returned pointer remains valid until the KeySet inside @p elektra is modified. Calls to elektraSet*() functions may
 *   cause such modifications. In any case, it becomes invalid when elektraClose() is called on @p elektra.
 */
Key * elektraFindKey (Elektra * elektra, const char * name, KDBType type)
{
	elektraSetLookupKey (elektra, name);
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
 * @param name    The (relative) name of the key.
 * @return the resolved version of the reference stored in the specified key (relative to the parent key of @p elektra)
 * or NULL, if the key was not found, or the reference resolves two a key not below the parent key. The empty string is
 * returned, if the value was the empty string (no resolution is attempted).
 *   The returned pointer becomes invalid when this function is called again (even with the same arguments). It is also
 *   invalidated when elektraFindReferenceArrayElement() or elektraClose() are called on @p elektra.
 */
const char * elektraFindReference (Elektra * elektra, const char * name)
{
	elektraSetLookupKey (elektra, name);
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
 * Reads the type metadata of a given key.
 *
 * @param elektra An Elektra instance.
 * @param keyname The name of the key whose type information shall be read.
 * @return the KDBType of the key
 */
KDBType elektraGetType (Elektra * elektra, const char * keyname)
{
	elektraSetLookupKey (elektra, keyname);
	const Key * key = elektraFindKey (elektra, keyname, NULL);
	const Key * metaKey = keyGetMeta (key, "type");
	return metaKey == NULL ? NULL : keyString (metaKey);
}

/**
 * Get the raw string value of a key.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the key.
 * @return the raw value of the specified key or NULL, if the key was not found
 *   The returned pointer remains valid until the internal state of @p elektra is modified.
 *   Calls to elektraSet*() functions may cause such modifications. In any case, it becomes
 *   invalid when elektraClose() is called on @p elektra.
 */
const char * elektraGetRawString (Elektra * elektra, const char * name)
{
	elektraSetLookupKey (elektra, name);
	Key * const resultKey = ksLookup (elektra->config, elektra->lookupKey, 0);
	return resultKey == NULL ? NULL : keyString (resultKey);
}

/**
 * Set the raw string value of a key.
 *
 * @param elektra The Elektra instance to use.
 * @param name    The (relative) name of the key.
 * @param value   The raw value to set.
 * @param type    The type to set in the metadata of the key.
 * @param error   Pointer to an ElektraError. Will be set in case saving fails.
 */
void elektraSetRawString (Elektra * elektra, const char * name, const char * value, KDBType type, ElektraError ** error)
{
	CHECK_ERROR (elektra, error);
	elektraSetLookupKey (elektra, name);
	Key * const key = keyDup (elektra->lookupKey, KEY_CP_NAME);
	keySetMeta (key, "type", type);
	keySetString (key, value);
	elektraSaveKey (elektra, key, error);
}

#define ELEKTRA_GET_VALUE(KEY_TO_VALUE, KDB_TYPE, elektra, keyname, result)                                                                \
	const Key * key = elektraFindKey (elektra, keyname, KDB_TYPE);                                                                     \
	if (key == NULL || !KEY_TO_VALUE (key, &result))                                                                                   \
	{                                                                                                                                  \
		elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE, keyname, keyString (key)));                        \
		result = 0;                                                                                                                \
	}

/**
 * Gets a string value.
 *
 * @param elektra The elektra instance to use.
 * @param keyname The (relative) name of the key to look up.
 * @return the string stored at the given key
 *   The returned pointer remains valid until the internal state of @p elektra is modified.
 *   Calls to elektraSet*() functions may cause such modifications. In any case, it becomes
 *   invalid when elektraClose() is called on @p elektra.
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
 * Gets an octet value.
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

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

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

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

#define ELEKTRA_SET_VALUE(VALUE_TO_STRING, KDB_TYPE, elektra, keyname, value, error)                                                       \
	CHECK_ERROR (elektra, error);                                                                                                      \
	char * string = VALUE_TO_STRING (value);                                                                                           \
	if (string == 0)                                                                                                                   \
	{                                                                                                                                  \
		*error = elektraErrorConversionToString (KDB_TYPE, keyname);                                                               \
		return;                                                                                                                    \
	}                                                                                                                                  \
	elektraSetRawString (elektra, keyname, string, KDB_TYPE, error);                                                                   \
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
	CHECK_ERROR (elektra, error);
	elektraSetRawString (elektra, keyname, value, KDB_TYPE_STRING, error);
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
 * Sets an octet value.
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

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

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

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

/**
 * @}
 */

#ifdef __cplusplus
};
#endif

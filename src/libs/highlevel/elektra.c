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
#include "kdberrors.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <memory.h>
#include <stdlib.h>

#include "stdio.h"

typedef const char * KDBType;
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

void setValueAsString (Elektra * elektra, const char * name, const char * value, KDBType type, ElektraError ** error);
static const char * getValueAsString (Elektra * elektra, const char * name, KDBType type);

void setArrayElementValueAsString (Elektra * elektra, const char * name, const char * value, KDBType type, size_t index,
				   ElektraError ** error);
static const char * getArrayElementValueAsString (Elektra * elektra, const char * name, KDBType type, size_t index);

/**
 * \defgroup highlevel High-level API
 * @{
 */

#undef ELEKTRA_TYPES
#define ELEKTRA_TYPES(X) \
    X(const char *, String, KDB_TYPE_STRING, KDB_STRING_TO_STRING, KDB_STRING_TO_STRING) \
    X(kdb_boolean_t, Boolean, KDB_TYPE_BOOLEAN, KDB_BOOLEAN_TO_STRING, KDB_STRING_TO_BOOLEAN) \
    X(kdb_char_t, Char, KDB_TYPE_CHAR, KDB_CHAR_TO_STRING, KDB_STRING_TO_CHAR) \
    X(kdb_octet_t, Octet, KDB_TYPE_OCTET, KDB_OCTET_TO_STRING, KDB_STRING_TO_OCTET) \
    X(kdb_short_t, Short, KDB_TYPE_SHORT, KDB_SHORT_TO_STRING, KDB_STRING_TO_SHORT) \
    X(kdb_unsigned_short_t, UnsignedShort, KDB_TYPE_UNSIGNED_SHORT, KDB_UNSIGNED_SHORT_TO_STRING, KDB_STRING_TO_UNSIGNED_SHORT) \
    X(kdb_long_t, Long, KDB_TYPE_LONG, KDB_LONG_TO_STRING, KDB_STRING_TO_LONG) \
    X(kdb_unsigned_long_t, UnsignedLong, KDB_TYPE_UNSIGNED_LONG, KDB_UNSIGNED_LONG_TO_STRING, KDB_STRING_TO_UNSIGNED_LONG) \
    X(kdb_long_long_t, LongLong, KDB_TYPE_LONG_LONG, KDB_LONG_LONG_TO_STRING, KDB_STRING_TO_LONG_LONG) \
    X(kdb_unsigned_long_long_t, UnsignedLongLong, KDB_TYPE_UNSIGNED_LONG_LONG, KDB_UNSIGNED_LONG_LONG_TO_STRING, KDB_STRING_TO_UNSIGNED_LONG_LONG) \
    X(kdb_float_t, Float, KDB_TYPE_FLOAT, KDB_FLOAT_TO_STRING, KDB_STRING_TO_FLOAT) \
    X(kdb_double_t, Double, KDB_TYPE_DOUBLE, KDB_DOUBLE_TO_STRING, KDB_STRING_TO_DOUBLE) \
    X(kdb_long_double_t, LongDouble, KDB_TYPE_LONG_DOUBLE, KDB_LONG_DOUBLE_TO_STRING, KDB_STRING_TO_LONG_DOUBLE)

#define ELEKTRA_DEFINITIONS(Type, typeName, KDB_TYPE, TO_STRING, TO_VALUE) \
    /** \
    * @param elektra The elektra instance initialized with the parent key. \
    * @param keyName The keyname to write to. The keyname is appended to the parent key. \
    * @param value The new value. \
    */ \
    ELEKTRA_SET_BY_STRING_SIGNATURE(Type, typeName) \
    { \
        setValueAsString (elektra, keyName, TO_STRING (value), KDB_TYPE, error); \
    } \
\
    ELEKTRA_SET_BY_TAG_SIGNATURE(Type, typeName) \
    { \
        setValueAsString (elektra, tag.keyName, TO_STRING (value), KDB_TYPE, error); \
    } \
\
    /** \
    * @param elektra The elektra instance initialized with the parent key. \
    * @param keyName The keyname to write to. The keyname is appended to the parent key. \
    * @param value The new value. \
    * @param index The array index of the desired element, starting with 0. \
    */ \
    ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE(Type, typeName) \
    { \
        setArrayElementValueAsString (elektra, keyName, TO_STRING (value), KDB_TYPE, index, error); \
    } \
\
    ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName) \
    { \
        setArrayElementValueAsString (elektra, tag.keyName, TO_STRING (value), KDB_TYPE, index, error); \
    } \
\
    /** \
    * @param elektra The elektra instance initialized with the parent key. \
    * @param name The keyname to look up. The keyname is appended to the parent key. \
    * @param index The array index of the desired element, starting with 0. \
    * @return The value stored at the given key and index. \
    */ \
    ELEKTRA_GET_SIGNATURE(Type, typeName) \
    { \
        return TO_VALUE (getValueAsString (elektra, keyName, KDB_TYPE)); \
    } \
\
    ELEKTRA_GET_BY_TAG_SIGNATURE(Type, typeName) \
    { \
        return TO_VALUE (getValueAsString (elektra, tag.keyName, KDB_TYPE)); \
    } \
\
    ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE(Type, typeName) \
    { \
        return TO_VALUE (getArrayElementValueAsString (elektra, keyName, KDB_TYPE, index)); \
    } \
\
    ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName) \
    { \
        return TO_VALUE (getArrayElementValueAsString (elektra, tag.keyName, KDB_TYPE, index)); \
    } \

ELEKTRA_TYPES(ELEKTRA_DEFINITIONS)

/**
 * Initializes a new Elektra instance.
 * @param application The parent key for your application.
 * @param defaults A KeySet containing default values. Passing NULL means "no default values".
 * @return An elektra instance initialized with the application.
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
 * @param elektra
 */
void elektraClose (Elektra * elektra)
{
	kdbClose (elektra->kdb, elektra->parentKey);
	keyDel (elektra->parentKey);
	ksDel (elektra->config);
	keyDel (elektra->lookupKey);

	elektraFree (elektra);
}

// Primitive setters

///**
// * @param elektra The elektra instance initialized with the parent key.
// * @param name The keyname to write to. The keyname is appended to the parent key.
// * @param value The new value.
// */
//void elektraSetString (Elektra * elektra, const char * name, const char * value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, value, KDB_TYPE_STRING, error);
//}

///**
// * @copydoc elektraSetString
// */
//void elektraSetBoolean (Elektra * elektra, const char * name, kdb_boolean_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_BOOLEAN_TO_STRING (value), KDB_TYPE_BOOLEAN, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetChar (Elektra * elektra, const char * name, kdb_char_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_CHAR_TO_STRING (value), KDB_TYPE_CHAR, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetOctet (Elektra * elektra, const char * name, kdb_octet_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_OCTET_TO_STRING (value), KDB_TYPE_OCTET, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetShort (Elektra * elektra, const char * name, kdb_short_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_SHORT_TO_STRING (value), KDB_TYPE_SHORT, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetUnsignedShort (Elektra * elektra, const char * name, kdb_unsigned_short_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_UNSIGNED_SHORT_TO_STRING (value), KDB_TYPE_UNSIGNED_SHORT, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetLong (Elektra * elektra, const char * name, kdb_long_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_LONG_TO_STRING (value), KDB_TYPE_LONG, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetUnsignedLong (Elektra * elektra, const char * name, kdb_unsigned_long_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_UNSIGNED_LONG_TO_STRING (value), KDB_TYPE_UNSIGNED_LONG, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetLongLong (Elektra * elektra, const char * name, kdb_long_long_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_LONG_LONG_TO_STRING (value), KDB_TYPE_LONG_LONG, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetUnsignedLongLong (Elektra * elektra, const char * name, kdb_unsigned_long_long_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_UNSIGNED_LONG_LONG_TO_STRING (value), KDB_TYPE_UNSIGNED_LONG_LONG, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetFloat (Elektra * elektra, const char * name, kdb_float_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_FLOAT_TO_STRING (value), KDB_TYPE_FLOAT, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetDouble (Elektra * elektra, const char * name, kdb_double_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_DOUBLE_TO_STRING (value), KDB_TYPE_DOUBLE, error);
//}
//
///**
// * @copydoc elektraSetString
// */
//void elektraSetLongDouble (Elektra * elektra, const char * name, kdb_long_double_t value, ElektraError ** error)
//{
//	setValueAsString (elektra, name, KDB_LONG_DOUBLE_TO_STRING (value), KDB_TYPE_LONG_DOUBLE, error);
//}
//
//// Array setters

///**
// * @param elektra The elektra instance initialized with the parent key.
// * @param name The keyname to write to. The keyname is appended to the parent key.
// * @param value The new value.
// * @param index The array index of the desired element, starting with 0.
// */
//void elektraSetStringArrayElement (Elektra * elektra, const char * name, const char * value, size_t index, ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, value, KDB_TYPE_STRING, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetBooleanArrayElement (Elektra * elektra, const char * name, kdb_boolean_t value, size_t index, ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_BOOLEAN_TO_STRING (value), KDB_TYPE_BOOLEAN, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetCharArrayElement (Elektra * elektra, const char * name, kdb_char_t value, size_t index, ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_CHAR_TO_STRING (value), KDB_TYPE_CHAR, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetOctetArrayElement (Elektra * elektra, const char * name, kdb_octet_t value, size_t index, ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_OCTET_TO_STRING (value), KDB_TYPE_OCTET, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetShortArrayElement (Elektra * elektra, const char * name, kdb_short_t value, size_t index, ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_SHORT_TO_STRING (value), KDB_TYPE_SHORT, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetUnsignedShortArrayElement (Elektra * elektra, const char * name, kdb_unsigned_short_t value, size_t index,
//					  ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_UNSIGNED_SHORT_TO_STRING (value), KDB_TYPE_UNSIGNED_SHORT, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetLongArrayElement (Elektra * elektra, const char * name, kdb_long_t value, size_t index, ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_LONG_TO_STRING (value), KDB_TYPE_LONG, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetUnsignedLongArrayElement (Elektra * elektra, const char * name, kdb_unsigned_long_t value, size_t index,
//					 ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_UNSIGNED_LONG_TO_STRING (value), KDB_TYPE_UNSIGNED_LONG, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetLongLongArrayElement (Elektra * elektra, const char * name, kdb_long_long_t value, size_t index, ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_LONG_LONG_TO_STRING (value), KDB_TYPE_LONG_LONG, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetUnsignedLongLongArrayElement (Elektra * elektra, const char * name, kdb_unsigned_long_long_t value, size_t index,
//					     ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_UNSIGNED_LONG_TO_STRING (value), KDB_TYPE_UNSIGNED_LONG_LONG, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetFloatArrayElement (Elektra * elektra, const char * name, kdb_float_t value, size_t index, ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_FLOAT_TO_STRING (value), KDB_TYPE_FLOAT, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetDoubleArrayElement (Elektra * elektra, const char * name, kdb_double_t value, size_t index, ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_DOUBLE_TO_STRING (value), KDB_TYPE_DOUBLE, index, error);
//}
//
///**
// * @copydoc elektraSetStringArrayElement
// */
//void elektraSetLongDoubleArrayElement (Elektra * elektra, const char * name, kdb_long_double_t value, size_t index, ElektraError ** error)
//{
//	setArrayElementValueAsString (elektra, name, KDB_LONG_DOUBLE_TO_STRING (value), KDB_TYPE_LONG_DOUBLE, index, error);
//}
//
//// Primitive getters
//
///**
// * @param elektra The elektra instance initialized with the parent key.
// * @param name The keyname to look up. The keyname is appended to the parent key.
// */
//const char * elektraGetStringByKeyname (Elektra * elektra, const char * name)
//{
//	return getValueAsString (elektra, name, KDB_TYPE_STRING);
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_BOOLEAN (getValueAsString (elektra, name, KDB_TYPE_BOOLEAN));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_char_t elektraGetChar (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_CHAR (getValueAsString (elektra, name, KDB_TYPE_CHAR));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_octet_t elektraGetOctet (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_OCTET (getValueAsString (elektra, name, KDB_TYPE_OCTET));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_short_t elektraGetShort (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_SHORT (getValueAsString (elektra, name, KDB_TYPE_SHORT));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_UNSIGNED_SHORT (getValueAsString (elektra, name, KDB_TYPE_UNSIGNED_SHORT));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_long_t elektraGetLong (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_LONG (getValueAsString (elektra, name, KDB_TYPE_LONG));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_UNSIGNED_LONG (getValueAsString (elektra, name, KDB_TYPE_UNSIGNED_LONG));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_LONG_LONG (getValueAsString (elektra, name, KDB_TYPE_LONG_LONG));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_UNSIGNED_LONG_LONG (getValueAsString (elektra, name, KDB_TYPE_UNSIGNED_LONG_LONG));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_float_t elektraGetFloat (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_FLOAT (getValueAsString (elektra, name, KDB_TYPE_FLOAT));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_double_t elektraGetDouble (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_DOUBLE (getValueAsString (elektra, name, KDB_TYPE_DOUBLE));
//}
//
///**
// * @copydoc elektraGetString
// */
//kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * name)
//{
//	return KDB_STRING_TO_LONG_DOUBLE (getValueAsString (elektra, name, KDB_TYPE_LONG_DOUBLE));
//}
//
//// Array getters
//
//
size_t elektraArraySize (Elektra * elektra, const char * name)
{
	Key * const key = generateLookupKey (elektra, name);

	KeySet * arrayKeys = elektraArrayGet (key, elektra->config);
	size_t size = (size_t)ksGetSize (arrayKeys);
	ksDel (arrayKeys);

	return size;
}
//
///**
//* @param elektra The elektra instance initialized with the parent key.
//* @param name The keyname to look up. The keyname is appended to the parent key.
//* @param index The array index of the desired element, starting with 0.
// * @return The value stored at the given key and index.
//*/
//const char * elektraGetStringArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return getArrayElementValueAsString (elektra, name, KDB_TYPE_STRING, index);
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_boolean_t elektraGetBooleanArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_BOOLEAN (getArrayElementValueAsString (elektra, name, KDB_TYPE_BOOLEAN, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_char_t elektraGetCharArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_CHAR (getArrayElementValueAsString (elektra, name, KDB_TYPE_CHAR, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_octet_t elektraGetOctetArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_OCTET (getArrayElementValueAsString (elektra, name, KDB_TYPE_OCTET, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_short_t elektraGetShortArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_SHORT (getArrayElementValueAsString (elektra, name, KDB_TYPE_SHORT, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_unsigned_short_t elektraGetUnsignedShortArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_UNSIGNED_SHORT (getArrayElementValueAsString (elektra, name, KDB_TYPE_UNSIGNED_SHORT, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_long_t elektraGetLongArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_LONG (getArrayElementValueAsString (elektra, name, KDB_TYPE_LONG, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_unsigned_long_t elektraGetUnsignedLongArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_UNSIGNED_LONG (getArrayElementValueAsString (elektra, name, KDB_TYPE_UNSIGNED_LONG, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_long_long_t elektraGetLongLongArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_LONG_LONG (getArrayElementValueAsString (elektra, name, KDB_TYPE_LONG_LONG, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_unsigned_long_long_t elektraGetUnsignedLongLongArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_UNSIGNED_LONG_LONG (getArrayElementValueAsString (elektra, name, KDB_TYPE_UNSIGNED_LONG_LONG, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_float_t elektraGetFloatArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_FLOAT (getArrayElementValueAsString (elektra, name, KDB_TYPE_FLOAT, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_double_t elektraGetDoubleArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_DOUBLE (getArrayElementValueAsString (elektra, name, KDB_TYPE_DOUBLE, index));
//}
//
///**
// * @copydoc elektraGetStringArrayElement
// */
//kdb_long_double_t elektraGetLongDoubleArrayElement (Elektra * elektra, const char * name, size_t index)
//{
//	return KDB_STRING_TO_LONG_DOUBLE (getArrayElementValueAsString (elektra, name, KDB_TYPE_LONG_DOUBLE, index));
//}
//
///**
// * @}
// */

// Private functions

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
			if (elektraErrorCode (kdbSetError) != ELEKTRA_ERROR_CONFLICT)
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

static const char * getValueAsString (Elektra * elektra, const char * name, KDBType type)
{
	Key * const key = generateLookupKey (elektra, name);

	return getKeyValue (elektra, key, type);
}

static const char * getArrayElementValueAsString (Elektra * elektra, const char * name, KDBType type, size_t index)
{
	Key * const key = generateArrayLookupKey (elektra, name, index);

	return getKeyValue (elektra, key, type);
}

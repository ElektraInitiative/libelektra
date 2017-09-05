/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_H
#define ELEKTRA_H

#include "elektra_error.h"
#include "elektra_conversion.h"
#include "kdb.h"
#include "kdbtypes.h"

/**
 * \defgroup highlevel High-level API
 * @{
 */

typedef struct _Elektra Elektra;

#define ELEKTRA_TAG(typeName) Elektra##typeName##Tag

#define ELEKTRA_SET_BY_TAG(typeName) __elektraSet##typeName##ByTag
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(typeName) __elektraSet##typeName##ArrayElementByTag

#define ELEKTRA_GET_BY_TAG(typeName) __elektraGet##typeName##ByTag
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(typeName) __elektraGet##typeName##ArrayElementByTag

#define ELEKTRA_SET_BY_TAG_SIGNATURE(Type, typeName)                                                                                       \
	void ELEKTRA_SET_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag, Type value, ElektraError * *error)
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName)                                                                         \
	void ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag, Type value, size_t index,         \
							  ElektraError * *error)

#define ELEKTRA_GET_BY_TAG_SIGNATURE(Type, typeName) Type ELEKTRA_GET_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag)
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName)                                                                         \
	Type ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag, size_t index)

struct ElektraTag
{
	char * keyName;
};

#define ELEKTRA_DECLARATIONS(Type, typeName)                                                                                               \
	typedef struct                                                                                                                     \
	{                                                                                                                                  \
		char * keyName;                                                                                                            \
	} ELEKTRA_TAG (typeName);                                                                                                          \
                                                                                                                                           \
	ELEKTRA_SET_BY_TAG_SIGNATURE (Type, typeName);                                                                                     \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (Type, typeName);                                                                       \
                                                                                                                                           \
	ELEKTRA_GET_BY_TAG_SIGNATURE (Type, typeName);                                                                                     \
	ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (Type, typeName);

#define ELEKTRA_DEFINITIONS(Type, typeName, KDB_TYPE, TO_STRING, TO_VALUE)                                                                 \
	ELEKTRA_SET_BY_TAG_SIGNATURE (Type, typeName)                                                                                      \
	{                                                                                                                                  \
		setValueAsString (elektra, tag.keyName, TO_STRING (value), KDB_TYPE, error);                                               \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (Type, typeName)                                                                        \
	{                                                                                                                                  \
		setArrayElementValueAsString (elektra, tag.keyName, TO_STRING (value), KDB_TYPE, index, error);                            \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_GET_BY_TAG_SIGNATURE (Type, typeName)                                                                                      \
	{                                                                                                                                  \
		return TO_VALUE (getValueAsString (elektra, tag.keyName, KDB_TYPE));                                                       \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (Type, typeName)                                                                        \
	{                                                                                                                                  \
		return TO_VALUE (getArrayElementValueAsString (elektra, tag.keyName, KDB_TYPE, index));                                    \
	}

// Elektra built-in types

#define ELEKTRA_TYPES(X)                                                                                                                   \
	X (const char *, String)                                                                                                           \
	X (kdb_boolean_t, Boolean)                                                                                                         \
	X (kdb_char_t, Char)                                                                                                               \
	X (kdb_octet_t, Octet)                                                                                                             \
	X (kdb_short_t, Short)                                                                                                             \
	X (kdb_unsigned_short_t, UnsignedShort)                                                                                            \
	X (kdb_long_t, Long)                                                                                                               \
	X (kdb_unsigned_long_t, UnsignedLong)                                                                                              \
	X (kdb_long_long_t, LongLong)                                                                                                      \
	X (kdb_unsigned_long_long_t, UnsignedLongLong)                                                                                     \
	X (kdb_float_t, Float)                                                                                                             \
	X (kdb_double_t, Double)                                                                                                           \
    X (kdb_long_double_t, LongDouble)                                                                                                  \
    X (int, Enum)

ELEKTRA_TYPES (ELEKTRA_DECLARATIONS)

// Setters

#define ELEKTRA_SET_VALUE_AS_STRING(typeName, KDB_TYPE, elektra, keynameOrTag, value, error)                                               \
	setValueAsString (elektra, _Generic((keynameOrTag), char *: keynameOrTag, ELEKTRA_TAG(typeName): ((ELEKTRA_TAG(typeName) *)&keynameOrTag)->keyName), value, KDB_TYPE, error)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keynameOrTag The keyname (or a codegenerated Tag) to write to. The keyname is appended to the parent key.
 * @param value The new value.
 * @param value Pass a reference to an ElektraError pointer.
 */
#define elektraSetString(elektra, keynameOrTag, value, error)                                                                              \
	ELEKTRA_SET_VALUE_AS_STRING (String, "string", elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetBoolean(elektra, keynameOrTag, value, error)                                                                             \
	ELEKTRA_SET_VALUE_AS_STRING (Boolean, "boolean", elektra, keynameOrTag, KDB_BOOLEAN_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetChar(elektra, keynameOrTag, value, error)                                                                                \
	ELEKTRA_SET_VALUE_AS_STRING (Char, "char", elektra, keynameOrTag, KDB_CHAR_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetOctet(elektra, keynameOrTag, value, error)                                                                               \
	ELEKTRA_SET_VALUE_AS_STRING (Octet, "octet", elektra, keynameOrTag, KDB_OCTET_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetShort(elektra, keynameOrTag, value, error)                                                                               \
	ELEKTRA_SET_VALUE_AS_STRING (Short, "short", elektra, keynameOrTag, KDB_SHORT_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetUnsignedShort(elektra, keynameOrTag, value, error)                                                                       \
	ELEKTRA_SET_VALUE_AS_STRING (UnsignedShort, "unsigned_short", elektra, keynameOrTag, KDB_UNSIGNED_SHORT_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetLong(elektra, keynameOrTag, value, error)                                                                                \
	ELEKTRA_SET_VALUE_AS_STRING (Long, "long", elektra, keynameOrTag, KDB_LONG_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetUnsignedLong(elektra, keynameOrTag, value, error)                                                                        \
	ELEKTRA_SET_VALUE_AS_STRING (UnsignedLong, "unsigned_long", elektra, keynameOrTag, KDB_UNSIGNED_LONG_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetLongLong(elektra, keynameOrTag, value, error)                                                                            \
	ELEKTRA_SET_VALUE_AS_STRING (LongLong, "long_long", elektra, keynameOrTag, KDB_LONG_LONG_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetUnsignedLongLong(elektra, keynameOrTag, value, error)                                                                    \
	ELEKTRA_SET_VALUE_AS_STRING (UnsignedLongLong, "unsigned_long_long", elektra, keynameOrTag,                                        \
				     KDB_UNSIGNED_LONG_LONG_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetFloat(elektra, keynameOrTag, value, error)                                                                               \
	ELEKTRA_SET_VALUE_AS_STRING (Float, "float", elektra, keynameOrTag, KDB_FLOAT_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetDouble(elektra, keynameOrTag, value, error)                                                                              \
	ELEKTRA_SET_VALUE_AS_STRING (Double, "double", elektra, keynameOrTag, KDB_DOUBLE_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetLongDouble(elektra, keynameOrTag, value, error)                                                                          \
	ELEKTRA_SET_VALUE_AS_STRING (LongDouble, "long_double", elektra, keynameOrTag, KDB_LONG_DOUBLE_TO_STRING (value), error)

/**
 * @copydoc elektraSetString
 */
//#define elektraSetEnum(elektra, keynameOrTag, value, error)                                                                               \
//ELEKTRA_SET_VALUE_AS_STRING (Enum, "enum", elektra, keynameOrTag, KDB_ENUM_TO_STRING (value), error)

// Getters

#define ELEKTRA_GET_VALUE_AS_STRING(typeName, KDB_TYPE, elektra, keynameOrTag)                                                             \
	getValueAsString (elektra, _Generic((keynameOrTag), char *: keynameOrTag, ELEKTRA_TAG(typeName): ((ELEKTRA_TAG(typeName) *)&keynameOrTag)->keyName), KDB_TYPE)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 * @return The value stored at the given key and index.
*/
#define elektraGetString(elektra, keynameOrTag) ELEKTRA_GET_VALUE_AS_STRING (String, "string", elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetBoolean(elektra, keynameOrTag)                                                                                           \
	KDB_STRING_TO_BOOLEAN (ELEKTRA_GET_VALUE_AS_STRING (Boolean, "boolean", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetChar(elektra, keynameOrTag) KDB_STRING_TO_CHAR (ELEKTRA_GET_VALUE_AS_STRING (Char, "char", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetOctet(elektra, keynameOrTag) KDB_STRING_TO_OCTET (ELEKTRA_GET_VALUE_AS_STRING (Octet, "octet", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetShort(elektra, keynameOrTag) KDB_STRING_TO_SHORT (ELEKTRA_GET_VALUE_AS_STRING (Short, "short", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetUnsignedShort(elektra, keynameOrTag)                                                                                     \
	KDB_STRING_TO_UNSIGNED_SHORT (ELEKTRA_GET_VALUE_AS_STRING (UnsignedShort, "unsigned_short", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetLong(elektra, keynameOrTag) KDB_STRING_TO_LONG (ELEKTRA_GET_VALUE_AS_STRING (Long, "long", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetUnsignedLong(elektra, keynameOrTag)                                                                                      \
	KDB_STRING_TO_UNSIGNED_LONG (ELEKTRA_GET_VALUE_AS_STRING (UnsignedLong, "unsigned_long", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetLongLong(elektra, keynameOrTag)                                                                                          \
	KDB_STRING_TO_LONG_LONG (ELEKTRA_GET_VALUE_AS_STRING (LongLong, "long_long", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetUnsignedLongLong(elektra, keynameOrTag)                                                                                  \
	KDB_STRING_TO_UNSIGNED_LONG_LONG (ELEKTRA_GET_VALUE_AS_STRING (UnsignedLongLong, "unsigned_long_long", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetFloat(elektra, keynameOrTag) KDB_STRING_TO_FLOAT (ELEKTRA_GET_VALUE_AS_STRING (Float, "float", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetDouble(elektra, keynameOrTag) KDB_STRING_TO_DOUBLE (ELEKTRA_GET_VALUE_AS_STRING (Double, "double", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
#define elektraGetLongDouble(elektra, keynameOrTag)                                                                                        \
	KDB_STRING_TO_LONG_DOUBLE (ELEKTRA_GET_VALUE_AS_STRING (LongDouble, "long_double", elektra, keynameOrTag))

/**
 * @copydoc elektraGetString
 */
//#define elektraGetEnum(elektra, keynameOrTag)                                                                                        \
//KDB_STRING_TO_ENUM (ELEKTRA_GET_VALUE_AS_STRING (Enum, "enum", elektra, keynameOrTag))

// Array-Setters

#define ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING(typeName, KDB_TYPE, elektra, keynameOrTag, value, index, error)                          \
	setArrayElementValueAsString (elektra, _Generic((keynameOrTag), char *: keynameOrTag, ELEKTRA_TAG(typeName): ((ELEKTRA_TAG(typeName) *)&keynameOrTag)->keyName), value, KDB_TYPE, index, error)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keynameOrTag The keyname (or a codegenerated Tag) to write to. The keyname is appended to the parent key.
 * @param value The new value.
 * @param value Pass a reference to an ElektraError pointer.
 */
#define elektraSetStringArrayElement(elektra, keynameOrTag, value, index, error)                                                           \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (String, "string", elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetBooleanArrayElement(elektra, keynameOrTag, value, index, error)                                                          \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (Boolean, "boolean", elektra, keynameOrTag, KDB_BOOLEAN_TO_STRING (value), index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetCharArrayElement(elektra, keynameOrTag, value, index, error)                                                             \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (Char, "char", elektra, keynameOrTag, KDB_CHAR_TO_STRING (value), index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetOctetArrayElement(elektra, keynameOrTag, value, index, error)                                                            \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (Octet, "octet", elektra, keynameOrTag, KDB_OCTET_TO_STRING (value), index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetShortArrayElement(elektra, keynameOrTag, value, index, error)                                                            \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (Short, "short", elektra, keynameOrTag, KDB_SHORT_TO_STRING (value), index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetUnsignedShortArrayElement(elektra, keynameOrTag, value, index, error)                                                    \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (UnsignedShort, "unsigned_short", elektra, keynameOrTag,                                 \
						   KDB_UNSIGNED_SHORT_TO_STRING (value), index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetLongArrayElement(elektra, keynameOrTag, value, index, error)                                                             \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (Long, "long", elektra, keynameOrTag, KDB_LONG_TO_STRING (value), index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetUnsignedLongArrayElement(elektra, keynameOrTag, value, index, error)                                                     \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (UnsignedLong, "unsigned_long", elektra, keynameOrTag,                                   \
						   KDB_UNSIGNED_LONG_TO_STRING (value), index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetLongLongArrayElement(elektra, keynameOrTag, value, index, error)                                                         \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (LongLong, "long_long", elektra, keynameOrTag, KDB_LONG_LONG_TO_STRING (value), index,   \
						   error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetUnsignedLongLongArrayElement(elektra, keynameOrTag, value, index, error)                                                 \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (UnsignedLongLong, "unsigned_long_long", elektra, keynameOrTag,                          \
						   KDB_UNSIGNED_LONG_LONG_TO_STRING (value), index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetFloatArrayElement(elektra, keynameOrTag, value, index, error)                                                            \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (Float, "float", elektra, keynameOrTag, KDB_FLOAT_TO_STRING (value), index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetDoubleArrayElement(elektra, keynameOrTag, value, index, error)                                                           \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (Double, "double", elektra, keynameOrTag, KDB_DOUBLE_TO_STRING (value), index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetLongDoubleArrayElement(elektra, keynameOrTag, value, index, error)                                                       \
	ELEKTRA_SET_ARRAY_ELEMENT_VALUE_AS_STRING (LongDouble, "longDouble", elektra, keynameOrTag, KDB_LONG_DOUBLE_TO_STRING (value),     \
						   index, error)

// Array-Getters

#define ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING(typeName, KDB_TYPE, elektra, keynameOrTag, index)                                        \
	getArrayElementValueAsString (elektra, _Generic((keynameOrTag), char *: keynameOrTag, ELEKTRA_TAG(typeName): ((ELEKTRA_TAG(typeName) *)&keynameOrTag)->keyName), KDB_TYPE, index)

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyName The keyname (or a codegenerated Tag) to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0. \
 * @return The value stored at the given key and index.
*/
#define elektraGetStringArrayElement(elektra, keynameOrTag, index)                                                                         \
	ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (String, "string", elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetBooleanArrayElement(elektra, keynameOrTag, index)                                                                        \
	KDB_STRING_TO_BOOLEAN (ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (Boolean, "boolean", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetCharArrayElement(elektra, keynameOrTag, index)                                                                           \
	KDB_STRING_TO_CHAR (ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (Char, "char", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetOctetArrayElement(elektra, keynameOrTag, index)                                                                          \
	KDB_STRING_TO_OCTET (ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (Octet, "octet", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetShortArrayElement(elektra, keynameOrTag, index)                                                                          \
	KDB_STRING_TO_SHORT (ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (Short, "short", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetUnsignedShortArrayElement(elektra, keynameOrTag, index)                                                                  \
	KDB_STRING_TO_UNSIGNED_SHORT (                                                                                                     \
		ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (UnsignedShort, "unsigned_short", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetLongArrayElement(elektra, keynameOrTag, index)                                                                           \
	KDB_STRING_TO_LONG (ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (Long, "long", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetUnsignedLongArrayElement(elektra, keynameOrTag, index)                                                                   \
	KDB_STRING_TO_UNSIGNED_LONG (                                                                                                      \
		ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (UnsignedLong, "unsigned_long", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetLongLongArrayElement(elektra, keynameOrTag, index)                                                                       \
	KDB_STRING_TO_LONG_LONG (ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (LongLong, "long_long", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetUnsignedLongLongArrayElement(elektra, keynameOrTag, index)                                                               \
	KDB_STRING_TO_UNSIGNED_LONG_LONG (                                                                                                 \
		ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (UnsignedLongLong, "unsigned_long_long", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetFloatArrayElement(elektra, keynameOrTag, index)                                                                          \
	KDB_STRING_TO_FLOAT (ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (Float, "float", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetDoubleArrayElement(elektra, keynameOrTag, index)                                                                         \
	KDB_STRING_TO_DOUBLE (ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (Double, "double", elektra, keynameOrTag, index))

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetLongDoubleArrayElement(elektra, keynameOrTag, index)                                                                     \
	KDB_STRING_TO_LONG_DOUBLE (ELEKTRA_GET_ARRAY_ELEMENT_VALUE_AS_STRING (LongDouble, "long_double", elektra, keynameOrTag, index))

void __elektraSetEnum(Elektra * elektra, char * name, int value, ElektraError ** error);
void __elektraSetEnumArrayElement(Elektra * elektra, char * name, int value, size_t index, ElektraError ** error);
int __elektraGetEnum(Elektra * elektra, char * keyName);
int __elektraGetEnumArrayElement(Elektra * elektra, char * keyName, int index);

Elektra * elektraOpen (const char * application, KeySet * defaults, ElektraError ** error);
void elektraClose (Elektra * elektra);
size_t elektraArraySize (Elektra * elektra, const char * keyName);

typedef const char * KDBType;

void setValueAsString (Elektra * elektra, const char * name, const char * value, KDBType type, ElektraError ** error);
const char * getValueAsString (Elektra * elektra, const char * name, KDBType type);

void setArrayElementValueAsString (Elektra * elektra, const char * name, const char * value, KDBType type, size_t index,
				   ElektraError ** error);
const char * getArrayElementValueAsString (Elektra * elektra, const char * name, KDBType type, size_t index);

// Generic Setters and Getters

#define ELEKTRA_GENERIC_SET_ENTRY(typeName) ELEKTRA_TAG (typeName) : ELEKTRA_SET_BY_TAG (typeName),

#define ELEKTRA_GENERIC_SET_ARRAY_ELEMENT_ENTRY(typeName) ELEKTRA_TAG (typeName) : ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (typeName),

#define ELEKTRA_GENERIC_GET_ENTRY(typeName) ELEKTRA_TAG (typeName) : ELEKTRA_GET_BY_TAG (typeName),

#define ELEKTRA_GENERIC_GET_ARRAY_ELEMENT_ENTRY(typeName) ELEKTRA_TAG (typeName) : ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (typeName),

#define ELEKTRA_TAG_NAMES_EXCEPT_STRING(X)                                                                                                 \
	X (Boolean)                                                                                                                        \
	X (Char)                                                                                                                           \
	X (Octet)                                                                                                                          \
	X (Short)                                                                                                                          \
	X (UnsignedShort)                                                                                                                  \
	X (Long)                                                                                                                           \
	X (UnsignedLong)                                                                                                                   \
	X (LongLong)                                                                                                                       \
	X (UnsignedLongLong)                                                                                                               \
	X (Float)                                                                                                                          \
	X (Double)                                                                                                                         \
	X (LongDouble)                                                                                                                     \
    X (Enum)

#include "elektra_generic.h"

#define ELEKTRA_TAG_NAMES_GEN(X)


/**
 * @}
 */

#endif // ELEKTRA_H

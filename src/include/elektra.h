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
#include "kdb.h"
#include "kdbtypes.h"

/**
 * \defgroup highlevel High-level API
 * @{
 */

typedef struct _Elektra Elektra;

#define ELEKTRA_TAG(typeName) Elektra##typeName##Tag

#define ELEKTRA_SET_BY_STRING(typeName) elektraSet##typeName##ByString
#define ELEKTRA_SET_BY_TAG(typeName) elektraSet##typeName##ByTag
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(typeName) elektraSet##typeName##ArrayElementByString
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(typeName) elektraSet##typeName##ArrayElementByTag

#define ELEKTRA_GET_BY_STRING(typeName) elektraGet##typeName##ByString
#define ELEKTRA_GET_BY_TAG(typeName) elektraGet##typeName##ByTag
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING(typeName) elektraGet##typeName##ArrayElementByString
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(typeName) elektraGet##typeName##ArrayElementByTag

#define ELEKTRA_SET_BY_STRING_SIGNATURE(Type, typeName)                                                                                    \
	void ELEKTRA_SET_BY_STRING (typeName) (Elektra * elektra, const char * keyName, Type value, ElektraError ** error)
#define ELEKTRA_SET_BY_TAG_SIGNATURE(Type, typeName)                                                                                       \
	void ELEKTRA_SET_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag, Type value, ElektraError * *error)
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING_SIGNATURE(Type, typeName)                                                                      \
	void ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING (typeName) (Elektra * elektra, const char * keyName, Type value, size_t index,            \
							     ElektraError ** error)
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName)                                                                         \
	void ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag, Type value, size_t index,         \
							  ElektraError * *error)

#define ELEKTRA_GET_BY_STRING_SIGNATURE(Type, typeName) Type ELEKTRA_GET_BY_STRING (typeName) (Elektra * elektra, const char * keyName)
#define ELEKTRA_GET_BY_TAG_SIGNATURE(Type, typeName) Type ELEKTRA_GET_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag)
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING_SIGNATURE(Type, typeName)                                                                      \
	Type ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING (typeName) (Elektra * elektra, const char * keyName, size_t index)
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName)                                                                         \
	Type ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag, size_t index)

#define ELEKTRA_DECLARATIONS(Type, typeName)                                                                                                \
	typedef struct                                                                                                                     \
	{                                                                                                                                  \
		char * keyName;                                                                                                            \
	} ELEKTRA_TAG (typeName);                                                                                                          \
                                                                                                                                           \
	ELEKTRA_SET_BY_STRING_SIGNATURE (Type, typeName);                                                                                  \
	ELEKTRA_SET_BY_TAG_SIGNATURE (Type, typeName);                                                                                     \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING_SIGNATURE (Type, typeName);                                                                    \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (Type, typeName);                                                                       \
                                                                                                                                           \
	ELEKTRA_GET_BY_STRING_SIGNATURE (Type, typeName);                                                                                  \
	ELEKTRA_GET_BY_TAG_SIGNATURE (Type, typeName);                                                                                     \
	ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING_SIGNATURE (Type, typeName);                                                                    \
	ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (Type, typeName);

#define ELEKTRA_DEFINITIONS(Type, typeName, KDB_TYPE, TO_STRING, TO_VALUE)                                                                 \
                                                                                                                                           \
	ELEKTRA_SET_BY_STRING_SIGNATURE (Type, typeName)                                                                                   \
	{                                                                                                                                  \
		setValueAsString (elektra, keyName, TO_STRING (value), KDB_TYPE, error);                                                   \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_SET_BY_TAG_SIGNATURE (Type, typeName)                                                                                      \
	{                                                                                                                                  \
		setValueAsString (elektra, tag.keyName, TO_STRING (value), KDB_TYPE, error);                                               \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING_SIGNATURE (Type, typeName)                                                                     \
	{                                                                                                                                  \
		setArrayElementValueAsString (elektra, keyName, TO_STRING (value), KDB_TYPE, index, error);                                \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (Type, typeName)                                                                        \
	{                                                                                                                                  \
		setArrayElementValueAsString (elektra, tag.keyName, TO_STRING (value), KDB_TYPE, index, error);                            \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_GET_BY_STRING_SIGNATURE (Type, typeName)                                                                                   \
	{                                                                                                                                  \
		return TO_VALUE (getValueAsString (elektra, keyName, KDB_TYPE));                                                           \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_GET_BY_TAG_SIGNATURE (Type, typeName)                                                                                      \
	{                                                                                                                                  \
		return TO_VALUE (getValueAsString (elektra, tag.keyName, KDB_TYPE));                                                       \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING_SIGNATURE (Type, typeName)                                                                     \
	{                                                                                                                                  \
		return TO_VALUE (getArrayElementValueAsString (elektra, keyName, KDB_TYPE, index));                                        \
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
	X (kdb_long_double_t, LongDouble)

ELEKTRA_TYPES (ELEKTRA_DECLARATIONS)

// Setters

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keynameOrTag The keyname (or a codegenerated Tag) to write to. The keyname is appended to the parent key.
 * @param value The new value.
 * @param value Pass a reference to an ElektraError pointer.
 */
#define elektraSetString(elektra, keynameOrTag, value, error)                                                                              \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(String), \
    ELEKTRA_TAG(String): ELEKTRA_SET_BY_TAG(String) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetBoolean(elektra, keynameOrTag, value, error)                                                                             \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Boolean), \
    ELEKTRA_TAG(Boolean): ELEKTRA_SET_BY_TAG(Boolean) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetChar(elektra, keynameOrTag, value, error)                                                                                \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Char), \
    ELEKTRA_TAG(Char): ELEKTRA_SET_BY_TAG(Char) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetOctet(elektra, keynameOrTag, value, error)                                                                               \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Octet), \
    ELEKTRA_TAG(Octet): ELEKTRA_SET_BY_TAG(Octet) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetShort(elektra, keynameOrTag, value, error)                                                                               \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Short), \
    ELEKTRA_TAG(Short): ELEKTRA_SET_BY_TAG(Short) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetUnsignedShort(elektra, keynameOrTag, value, error)                                                                       \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(UnsignedShort), \
    ELEKTRA_TAG(UnsignedShort): ELEKTRA_SET_BY_TAG(UnsignedShort) \
    )(elektra, keynameOrTag, value, error)

#define elektraSetLong(elektra, keynameOrTag, value, error)                                                                                \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Long), \
    ELEKTRA_TAG(Long): ELEKTRA_SET_BY_TAG(Long) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetUnsignedLong(elektra, keynameOrTag, value, error)                                                                        \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(UnsignedLong), \
    ELEKTRA_TAG(UnsignedLong): ELEKTRA_SET_BY_TAG(UnsignedLong) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetLongLong(elektra, keynameOrTag, value, error)                                                                            \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(LongLong), \
    ELEKTRA_TAG(LongLong): ELEKTRA_SET_BY_TAG(LongLong) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetUnsignedLongLong(elektra, keynameOrTag, value, error)                                                                    \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(UnsignedLongLong), \
    ELEKTRA_TAG(UnsignedLongLong): ELEKTRA_SET_BY_TAG(UnsignedLongLong) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetFloat(elektra, keynameOrTag, value, error)                                                                               \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Float), \
    ELEKTRA_TAG(Float): ELEKTRA_SET_BY_TAG(Float) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetDouble(elektra, keynameOrTag, value, error)                                                                              \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Double), \
    ELEKTRA_TAG(Double): ELEKTRA_SET_BY_TAG(Double) \
    )(elektra, keynameOrTag, value, error)

/**
 * @copydoc elektraSetString
 */
#define elektraSetLongDouble(elektra, keynameOrTag, value, error)                                                                          \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_BY_STRING(LongDouble), \
    ELEKTRA_TAG(LongDouble): ELEKTRA_SET_BY_TAG(LongDouble) \
    )(elektra, keynameOrTag, value, error)

// Getters

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 * @return The value stored at the given key and index.
*/
#define elektraGetString(elektra, keynameOrTag)                                                                                            \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_BY_STRING(String), \
    ELEKTRA_TAG(String): ELEKTRA_GET_BY_TAG(String) \
    )(elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetBoolean(elektra, keynameOrTag)                                                                                           \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_BY_STRING(Boolean), \
    ELEKTRA_TAG(Boolean): ELEKTRA_GET_BY_TAG(Boolean) \
    )(elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetChar(elektra, keynameOrTag)                                                                                              \
	_Generic((keynameOrTag), char * : ELEKTRA_GET_BY_STRING (Char), ELEKTRA_TAG (Char) : ELEKTRA_GET_BY_TAG (Char)) (elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetOctet(elektra, keynameOrTag)                                                                                             \
	_Generic((keynameOrTag), char * : ELEKTRA_GET_BY_STRING (Octet), ELEKTRA_TAG (Octet) : ELEKTRA_GET_BY_TAG (Octet)) (elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetShort(elektra, keynameOrTag)                                                                                             \
	_Generic((keynameOrTag), char * : ELEKTRA_GET_BY_STRING (Short), ELEKTRA_TAG (Short) : ELEKTRA_GET_BY_TAG (Short)) (elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetUnsignedShort(elektra, keynameOrTag)                                                                                     \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_BY_STRING(UnsignedShort), \
    ELEKTRA_TAG(UnsignedShort): ELEKTRA_GET_BY_TAG(UnsignedShort) \
    )(elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetLong(elektra, keynameOrTag)                                                                                              \
	_Generic((keynameOrTag), char * : ELEKTRA_GET_BY_STRING (Long), ELEKTRA_TAG (Long) : ELEKTRA_GET_BY_TAG (Long)) (elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetUnsignedLong(elektra, keynameOrTag)                                                                                      \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_BY_STRING(UnsignedLong), \
    ELEKTRA_TAG(UnsignedLong): ELEKTRA_GET_BY_TAG(UnsignedLong) \
    )(elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetLongLong(elektra, keynameOrTag)                                                                                          \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_BY_STRING(LongLong), \
    ELEKTRA_TAG(LongLong): ELEKTRA_GET_BY_TAG(LongLong) \
    )(elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetUnsignedLongLong(elektra, keynameOrTag)                                                                                  \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_BY_STRING(UnsignedLongLong), \
    ELEKTRA_TAG(UnsignedLongLong): ELEKTRA_GET_BY_TAG(UnsignedLongLong) \
    )(elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetFloat(elektra, keynameOrTag)                                                                                             \
	_Generic((keynameOrTag), char * : ELEKTRA_GET_BY_STRING (Float), ELEKTRA_TAG (Float) : ELEKTRA_GET_BY_TAG (Float)) (elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetDouble(elektra, keynameOrTag)                                                                                            \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_BY_STRING(Double), \
    ELEKTRA_TAG(Double): ELEKTRA_GET_BY_TAG(Double) \
    )(elektra, keynameOrTag)

/**
 * @copydoc elektraGetString
 */
#define elektraGetLongDouble(elektra, keynameOrTag)                                                                                        \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_BY_STRING(LongDouble), \
    ELEKTRA_TAG(LongDouble): ELEKTRA_GET_BY_TAG(LongDouble) \
    )(elektra, keynameOrTag)

// Array-Setters

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keynameOrTag The keyname (or a codegenerated Tag) to write to. The keyname is appended to the parent key.
 * @param value The new value.
 * @param value Pass a reference to an ElektraError pointer.
 */
#define elektraSetStringArrayElement(elektra, keynameOrTag, value, index, error)                                                           \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(String), \
    ELEKTRA_TAG(String): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(String) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetBooleanArrayElement(elektra, keynameOrTag, value, index, error)                                                          \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(Boolean), \
    ELEKTRA_TAG(Boolean): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(Boolean) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetCharArrayElement(elektra, keynameOrTag, value, index, error)                                                             \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(Char), \
    ELEKTRA_TAG(Char): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(Char) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetOctetArrayElement(elektra, keynameOrTag, value, index, error)                                                            \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(Octet), \
    ELEKTRA_TAG(Octet): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(Octet) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetShortArrayElement(elektra, keynameOrTag, value, index, error)                                                            \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(Short), \
    ELEKTRA_TAG(Short): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(Short) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetUnsignedShortArrayElement(elektra, keynameOrTag, value, index, error)                                                    \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(UnsignedShort), \
    ELEKTRA_TAG(UnsignedShort): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(UnsignedShort) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetLongArrayElement(elektra, keynameOrTag, value, index, error)                                                             \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(Long), \
    ELEKTRA_TAG(Long): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(Long) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetUnsignedLongArrayElement(elektra, keynameOrTag, value, index, error)                                                     \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(UnsignedLong), \
    ELEKTRA_TAG(UnsignedLong): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(UnsignedLong) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetLongLongArrayElement(elektra, keynameOrTag, value, index, error)                                                         \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(LongLong), \
    ELEKTRA_TAG(LongLong): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(LongLong) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetUnsignedLongLongArrayElement(elektra, keynameOrTag, value, index, error)                                                 \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(UnsignedLongLong), \
    ELEKTRA_TAG(UnsignedLongLong): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(UnsignedLongLong) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetFloatArrayElement(elektra, keynameOrTag, value, index, error)                                                            \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(Float), \
    ELEKTRA_TAG(Float): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(Float) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetDoubleArrayElement(elektra, keynameOrTag, value, index, error)                                                           \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(Double), \
    ELEKTRA_TAG(Double): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(Double) \
    )(elektra, keynameOrTag, value, index, error)

/**
 * @copydoc elektraSetStringArrayElement
 */
#define elektraSetLongDoubleArrayElement(elektra, keynameOrTag, value, index, error)                                                       \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_SET_ARRAY_ELEMENT_BY_STRING(LongDouble), \
    ELEKTRA_TAG(LongDouble): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(LongDouble) \
    )(elektra, keynameOrTag, value, index, error)

// Array-Getters

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyName The keyname (or a codegenerated Tag) to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0. \
 * @return The value stored at the given key and index.
*/
#define elektraGetStringArrayElement(elektra, keynameOrTag, index)                                                                         \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING(String), \
    ELEKTRA_TAG(String): ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(String) \
    )(elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetBooleanArrayElement(elektra, keynameOrTag, index)                                                                        \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING(Boolean), \
    ELEKTRA_TAG(Boolean): ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(Boolean) \
    )(elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetCharArrayElement(elektra, keynameOrTag, index)                                                                           \
	_Generic((keynameOrTag), char * : ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING (Char), ELEKTRA_TAG (Char) : ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (Char)) (elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetOctetArrayElement(elektra, keynameOrTag, index)                                                                          \
	_Generic((keynameOrTag), char * : ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING (Octet), ELEKTRA_TAG (Octet) : ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (Octet)) (elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetShortArrayElement(elektra, keynameOrTag, index)                                                                          \
	_Generic((keynameOrTag), char * : ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING (Short), ELEKTRA_TAG (Short) : ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (Short)) (elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetUnsignedShortArrayElement(elektra, keynameOrTag, index)                                                                  \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING(UnsignedShort), \
    ELEKTRA_TAG(UnsignedShort): ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(UnsignedShort) \
    )(elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetLongArrayElement(elektra, keynameOrTag, index)                                                                           \
	_Generic((keynameOrTag), char * : ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING (Long), ELEKTRA_TAG (Long) : ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (Long)) (elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetUnsignedLongArrayElement(elektra, keynameOrTag, index)                                                                   \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING(UnsignedLong), \
    ELEKTRA_TAG(UnsignedLong): ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(UnsignedLong) \
    )(elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetLongLongArrayElement(elektra, keynameOrTag, index)                                                                       \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING(LongLong), \
    ELEKTRA_TAG(LongLong): ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(LongLong) \
    )(elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetUnsignedLongLongArrayElement(elektra, keynameOrTag, index)                                                               \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING(UnsignedLongLong), \
    ELEKTRA_TAG(UnsignedLongLong): ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(UnsignedLongLong) \
    )(elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetFloatArrayElement(elektra, keynameOrTag, index)                                                                          \
	_Generic((keynameOrTag), char * : ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING (Float), ELEKTRA_TAG (Float) : ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (Float)) (elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetDoubleArrayElement(elektra, keynameOrTag, index)                                                                         \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING(Double), \
    ELEKTRA_TAG(Double): ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(Double) \
    )(elektra, keynameOrTag, index)

/**
 * @copydoc elektraGetStringArrayElement
 */
#define elektraGetLongDoubleArrayElement(elektra, keynameOrTag, index)                                                                     \
	_Generic((keynameOrTag), \
    char *: ELEKTRA_GET_ARRAY_ELEMENT_BY_STRING(LongDouble), \
    ELEKTRA_TAG(LongDouble): ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(LongDouble) \
    )(elektra, keynameOrTag, index)

Elektra * elektraOpen (const char * application, KeySet * defaults, ElektraError ** error);
void elektraClose (Elektra * elektra);
size_t elektraArraySize (Elektra * elektra, const char * keyName);


// Generic Setters and Getters

#define ELEKTRA_GENERIC_SET_ENTRY(typeName) ELEKTRA_TAG (typeName) : ELEKTRA_SET_BY_TAG (typeName),

#define ELEKTRA_GENERIC_SET_ARRAY_ELEMENT_ENTRY(typeName) ELEKTRA_TAG (typeName) : ELEKTRA_SET_BY_TAG (typeName),

#define ELEKTRA_GENERIC_GET_ENTRY(typeName) ELEKTRA_TAG (typeName) : ELEKTRA_GET_BY_TAG (typeName),

#define ELEKTRA_GENERIC_GET_ARRAY_ELEMENT_ENTRY(typeName) ELEKTRA_TAG (typeName) : ELEKTRA_GET_BY_TAG (typeName),

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
	X (LongDouble)

#include "elektra_generic.h"

#define ELEKTRA_TAG_NAMES_GEN(X)                                                                                                 \


/**
 * @}
 */

#endif // ELEKTRA_H

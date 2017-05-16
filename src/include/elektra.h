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

typedef struct _Elektra Elektra;

#define ELEKTRA_TAG(typeName) Elektra##typeName##Tag

#define ELEKTRA_SET_BY_STRING(typeName) elektraSetByString##typeName
#define ELEKTRA_SET_BY_TAG(typeName) elektraSet##typeName##ByTag
#define ELEKTRA_SET_ARRAY_ELEMENT(typeName) elektraSet##typeName##ArrayElement
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(typeName) elektraSet##typeName##ArrayElementByTag

#define ELEKTRA_GET_BY_STRING(typeName) elektraGetByString##typeName
#define ELEKTRA_GET_BY_TAG(typeName) elektraGet##typeName##ByTag
#define ELEKTRA_GET_ARRAY_ELEMENT(typeName) elektraGet##typeName##ArrayElement
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(typeName) elektraGet##typeName##ArrayElementByTag

#define ELEKTRA_SET_BY_STRING_SIGNATURE(Type, typeName)                                                                                    \
	void ELEKTRA_SET_BY_STRING (typeName) (Elektra * elektra, const char * keyName, Type value, ElektraError ** error)
#define ELEKTRA_SET_BY_TAG_SIGNATURE(Type, typeName)                                                                                       \
	void ELEKTRA_SET_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag, Type value, ElektraError * *error)
#define ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE(Type, typeName)                                                                                \
	void ELEKTRA_SET_ARRAY_ELEMENT (typeName) (Elektra * elektra, const char * keyName, Type value, size_t index, ElektraError ** error)
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName)                                                                         \
	void ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag, Type value, size_t index,         \
							  ElektraError * *error)

#define ELEKTRA_GET_SIGNATURE(Type, typeName) Type ELEKTRA_GET_BY_STRING (typeName) (Elektra * elektra, const char * keyName)
#define ELEKTRA_GET_BY_TAG_SIGNATURE(Type, typeName) Type ELEKTRA_GET_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag)
#define ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE(Type, typeName)                                                                                \
	Type ELEKTRA_GET_ARRAY_ELEMENT (typeName) (Elektra * elektra, const char * keyName, size_t index)
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName)                                                                         \
	Type ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (typeName) (Elektra * elektra, ELEKTRA_TAG (typeName) tag, size_t index)

#define ELEKTRA_DECLARATION(Type, typeName)                                                                                                \
	typedef struct                                                                                                                     \
	{                                                                                                                                  \
		char * keyName;                                                                                                            \
	} ELEKTRA_TAG (typeName);                                                                                                          \
                                                                                                                                           \
	ELEKTRA_SET_BY_STRING_SIGNATURE (Type, typeName);                                                                                  \
	ELEKTRA_SET_BY_TAG_SIGNATURE (Type, typeName);                                                                                     \
	ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE (Type, typeName);                                                                              \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (Type, typeName);                                                                       \
                                                                                                                                           \
	ELEKTRA_GET_SIGNATURE (Type, typeName);                                                                                            \
	ELEKTRA_GET_BY_TAG_SIGNATURE (Type, typeName);                                                                                     \
	ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE (Type, typeName);                                                                              \
	ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (Type, typeName);


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

ELEKTRA_TYPES (ELEKTRA_DECLARATION)
#undef ELEKTRA_DECLARATION

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

#define elektraSet(ELEKTRA, TAG, VALUE, ERROR)                                                                                             \
	_Generic((TAG), ELEKTRA_TAG_NAMES_EXCEPT_STRING (ELEKTRA_GENERIC_SET_ENTRY) ELEKTRA_TAG (String)                                   \
		 : ELEKTRA_SET_BY_TAG (String)) (ELEKTRA, TAG, VALUE, ERROR)

#define elektraSetArrayElement(ELEKTRA, TAG, VALUE, INDEX, ERROR)                                                                          \
	_Generic((TAG), ELEKTRA_TAG_NAMES_EXCEPT_STRING (ELEKTRA_GENERIC_SET_ARRAY_ELEMENT_ENTRY) ELEKTRA_TAG (String)                     \
		 : ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (String)) (ELEKTRA, TAG, VALUE, INDEX, ERROR)

#define elektraGet(ELEKTRA, TAG)                                                                                                           \
	_Generic((TAG), ELEKTRA_TAG_NAMES_EXCEPT_STRING (ELEKTRA_GENERIC_GET_ENTRY) ELEKTRA_TAG (String)                                   \
		 : ELEKTRA_GET_BY_TAG (String)) (ELEKTRA, TAG)

#define elektraGetArrayElement(ELEKTRA, TAG, INDEX)                                                                                        \
	_Generic((TAG), ELEKTRA_TAG_NAMES_EXCEPT_STRING (ELEKTRA_GENERIC_GET_ARRAY_ELEMENT_ENTRY) ELEKTRA_TAG (String)                     \
		 : ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (String)) (ELEKTRA, TAG, INDEX)

// Setters

#define elektraSetString(elektra, keyOrTag, value, error)                                                                                  \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(String), \
    ELEKTRA_TAG(String): ELEKTRA_SET_BY_TAG(String) \
    )(elektra, keyOrTag, value, error)

#define elektraSetBoolean(elektra, keyOrTag, value, error)                                                                                 \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Boolean), \
    ELEKTRA_TAG(Boolean): ELEKTRA_SET_BY_TAG(Boolean) \
    )(elektra, keyOrTag, value, error)

#define elektraSetChar(elektra, keyOrTag, value, error)                                                                                    \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Char), \
    ELEKTRA_TAG(Char): ELEKTRA_SET_BY_TAG(Char) \
    )(elektra, keyOrTag, value, error)

#define elektraSetOctet(elektra, keyOrTag, value, error)                                                                                   \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Octet), \
    ELEKTRA_TAG(Octet): ELEKTRA_SET_BY_TAG(Octet) \
    )(elektra, keyOrTag, value, error)

#define elektraSetShort(elektra, keyOrTag, value, error)                                                                                   \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Short), \
    ELEKTRA_TAG(Short): ELEKTRA_SET_BY_TAG(Short) \
    )(elektra, keyOrTag, value, error)

#define elektraSetUnsignedShort(elektra, keyOrTag, value, error)                                                                           \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(UnsignedShort), \
    ELEKTRA_TAG(UnsignedShort): ELEKTRA_SET_BY_TAG(UnsignedShort) \
    )(elektra, keyOrTag, value, error)

#define elektraSetLong(elektra, keyOrTag, value, error)                                                                                    \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Long), \
    ELEKTRA_TAG(Long): ELEKTRA_SET_BY_TAG(Long) \
    )(elektra, keyOrTag, value, error)

#define elektraSetUnsignedLong(elektra, keyOrTag, value, error)                                                                            \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(UnsignedLong), \
    ELEKTRA_TAG(UnsignedLong): ELEKTRA_SET_BY_TAG(UnsignedLong) \
    )(elektra, keyOrTag, value, error)

#define elektraSetLongLong(elektra, keyOrTag, value, error)                                                                                \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(LongLong), \
    ELEKTRA_TAG(LongLong): ELEKTRA_SET_BY_TAG(LongLong) \
    )(elektra, keyOrTag, value, error)

#define elektraSetUnsignedLongLong(elektra, keyOrTag, value, error)                                                                        \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(UnsignedLongLong), \
    ELEKTRA_TAG(UnsignedLongLong): ELEKTRA_SET_BY_TAG(UnsignedLongLong) \
    )(elektra, keyOrTag, value, error)

#define elektraSetFloat(elektra, keyOrTag, value, error)                                                                                   \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Float), \
    ELEKTRA_TAG(Float): ELEKTRA_SET_BY_TAG(Float) \
    )(elektra, keyOrTag, value, error)

#define elektraSetDouble(elektra, keyOrTag, value, error)                                                                                  \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(Double), \
    ELEKTRA_TAG(Double): ELEKTRA_SET_BY_TAG(Double) \
    )(elektra, keyOrTag, value, error)

#define elektraSetLongDouble(elektra, keyOrTag, value, error)                                                                              \
	_Generic((keyOrTag), \
    char *: ELEKTRA_SET_BY_STRING(LongDouble), \
    ELEKTRA_TAG(LongDouble): ELEKTRA_SET_BY_TAG(LongDouble) \
    )(elektra, keyOrTag, value, error)

// Getters

#define elektraGetString(elektra, keyOrTag)                                                                                                \
	_Generic((keyOrTag), \
    char *: ELEKTRA_GET_BY_STRING(String), \
    ELEKTRA_TAG(String): ELEKTRA_GET_BY_TAG(String) \
    )(elektra, keyOrTag)

#define elektraGetBoolean(elektra, keyOrTag)                                                                                               \
	_Generic((keyOrTag), \
    char *: ELEKTRA_GET_BY_STRING(Boolean), \
    ELEKTRA_TAG(Boolean): ELEKTRA_GET_BY_TAG(Boolean) \
    )(elektra, keyOrTag)

#define elektraGetChar(elektra, keyOrTag)                                                                                                  \
	_Generic((keyOrTag), char * : ELEKTRA_GET_BY_STRING (Char), ELEKTRA_TAG (Char) : ELEKTRA_GET_BY_TAG (Char)) (elektra, keyOrTag)

#define elektraGetOctet(elektra, keyOrTag)                                                                                                 \
	_Generic((keyOrTag), char * : ELEKTRA_GET_BY_STRING (Octet), ELEKTRA_TAG (Octet) : ELEKTRA_GET_BY_TAG (Octet)) (elektra, keyOrTag)

#define elektraGetShort(elektra, keyOrTag)                                                                                                 \
	_Generic((keyOrTag), char * : ELEKTRA_GET_BY_STRING (Short), ELEKTRA_TAG (Short) : ELEKTRA_GET_BY_TAG (Short)) (elektra, keyOrTag)

#define elektraGetUnsignedShort(elektra, keyOrTag)                                                                                         \
	_Generic((keyOrTag), \
    char *: ELEKTRA_GET_BY_STRING(UnsignedShort), \
    ELEKTRA_TAG(UnsignedShort): ELEKTRA_GET_BY_TAG(UnsignedShort) \
    )(elektra, keyOrTag)

#define elektraGetLong(elektra, keyOrTag)                                                                                                  \
	_Generic((keyOrTag), char * : ELEKTRA_GET_BY_STRING (Long), ELEKTRA_TAG (Long) : ELEKTRA_GET_BY_TAG (Long)) (elektra, keyOrTag)

#define elektraGetUnsignedLong(elektra, keyOrTag)                                                                                          \
	_Generic((keyOrTag), \
    char *: ELEKTRA_GET_BY_STRING(UnsignedLong), \
    ELEKTRA_TAG(UnsignedLong): ELEKTRA_GET_BY_TAG(UnsignedLong) \
    )(elektra, keyOrTag)

#define elektraGetLongLong(elektra, keyOrTag)                                                                                              \
	_Generic((keyOrTag), \
    char *: ELEKTRA_GET_BY_STRING(LongLong), \
    ELEKTRA_TAG(LongLong): ELEKTRA_GET_BY_TAG(LongLong) \
    )(elektra, keyOrTag)

#define elektraGetUnsignedLongLong(elektra, keyOrTag)                                                                                      \
	_Generic((keyOrTag), \
    char *: ELEKTRA_GET_BY_STRING(UnsignedLongLong), \
    ELEKTRA_TAG(UnsignedLongLong): ELEKTRA_GET_BY_TAG(UnsignedLongLong) \
    )(elektra, keyOrTag)

#define elektraGetFloat(elektra, keyOrTag)                                                                                                 \
	_Generic((keyOrTag), char * : ELEKTRA_GET_BY_STRING (Float), ELEKTRA_TAG (Float) : ELEKTRA_GET_BY_TAG (Float)) (elektra, keyOrTag)

#define elektraGetDouble(elektra, keyOrTag)                                                                                                \
	_Generic((keyOrTag), \
    char *: ELEKTRA_GET_BY_STRING(Double), \
    ELEKTRA_TAG(Double): ELEKTRA_GET_BY_TAG(Double) \
    )(elektra, keyOrTag)

#define elektraGetLongDouble(elektra, keyOrTag)                                                                                            \
	_Generic((keyOrTag), \
    char *: ELEKTRA_GET_BY_STRING(LongDouble), \
    ELEKTRA_TAG(LongDouble): ELEKTRA_GET_BY_TAG(LongDouble) \
    )(elektra, keyOrTag)

Elektra * elektraOpen (const char * application, KeySet * defaults, ElektraError ** error);
void elektraClose (Elektra * elektra);
size_t elektraArraySize (Elektra * elektra, const char * keyName);

#endif // ELEKTRA_H

/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_H
#define ELEKTRA_H

#include <elektra/error.h>
#include <elektra/errors.h>
#include <elektra/types.h>
#include <kdb.h>
#include <kdbtypes.h>

// region Helpers for Tag Macros
/**************************************
 *
 * Helpers for Tag Macros
 *
 **************************************/

#ifdef __cplusplus
#define ELEKTRA_CAST(type, expression) static_cast<type> (expression)
#else
#define ELEKTRA_CAST(type, expression) (type) (expression)
#endif

#define ELEKTRA_TAG_TYPE(typeName) struct Elektra##typeName##Tag

#define ELEKTRA_SET_BY_TAG(typeName) elektraGenInternalSet##typeName##ByTag
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(typeName) elektraGenInternalSet##typeName##ArrayElementByTag

#define ELEKTRA_GET_BY_TAG(typeName) elektraGenInternalGet##typeName##ByTag
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(typeName) elektraGenInternalGet##typeName##ArrayElementByTag

#define ELEKTRA_GET_BY_TAG_PARAMS(typeName) (Elektra * elektra, const ELEKTRA_TAG_TYPE (typeName) * tag)
#define ELEKTRA_GET_BY_TAG_SIGNATURE(cType, typeName) cType ELEKTRA_GET_BY_TAG (typeName) ELEKTRA_GET_BY_TAG_PARAMS (typeName)
#define ELEKTRA_GET_BY_TAG_POINTER(name, cType, typeName) cType (*name) ELEKTRA_GET_BY_TAG_PARAMS (typeName)

#define ELEKTRA_GET_OUT_PTR_BY_TAG_PARAMS(cType, typeName) (Elektra * elektra, const ELEKTRA_TAG_TYPE (typeName) * tag, cType * result)
#define ELEKTRA_GET_OUT_PTR_BY_TAG_SIGNATURE(cType, typeName)                                                                              \
	void ELEKTRA_GET_BY_TAG (typeName) ELEKTRA_GET_OUT_PTR_BY_TAG_PARAMS (cType, typeName)
#define ELEKTRA_GET_OUT_PTR_BY_TAG_POINTER(name, cType, typeName) void(*name) ELEKTRA_GET_OUT_PTR_BY_TAG_PARAMS (cType, typeName)

#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_PARAMS(typeName)                                                                                  \
	(Elektra * elektra, const ELEKTRA_TAG_TYPE (typeName) * tag, kdb_long_long_t index)
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(cType, typeName)                                                                        \
	cType ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (typeName) ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_PARAMS (typeName)
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_POINTER(name, cType, typeName) cType (*name) ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_PARAMS (typeName)

#define ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_BY_TAG_PARAMS(cType, typeName)                                                                   \
	(Elektra * elektra, const ELEKTRA_TAG_TYPE (typeName) * tag, kdb_long_long_t index, cType * result)
#define ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_BY_TAG_SIGNATURE(cType, typeName)                                                                \
	cType ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (typeName) ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_BY_TAG_PARAMS (cType, typeName)
#define ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_BY_TAG_POINTER(name, cType, typeName)                                                            \
	void(*name) ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_BY_TAG_PARAMS (cType, typeName)

#define ELEKTRA_SET_BY_TAG_PARAMS(cType, typeName)                                                                                         \
	(Elektra * elektra, const ELEKTRA_TAG_TYPE (typeName) * tag, cType value, ElektraError * *error)
#define ELEKTRA_SET_BY_TAG_SIGNATURE(cType, typeName) void ELEKTRA_SET_BY_TAG (typeName) ELEKTRA_SET_BY_TAG_PARAMS (cType, typeName)
#define ELEKTRA_SET_BY_TAG_POINTER(name, cType, typeName) void(*name) ELEKTRA_SET_BY_TAG_PARAMS (cType, typeName)

#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_PARAMS(cType, typeName)                                                                           \
	(Elektra * elektra, const ELEKTRA_TAG_TYPE (typeName) * tag, kdb_long_long_t index, cType value, ElektraError * *error)
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(cType, typeName)                                                                        \
	void ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (typeName) ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_PARAMS (cType, typeName)
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_POINTER(name, cType, typeName)                                                                    \
	void(*name) ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_PARAMS (cType, typeName)

#define ELEKTRA_TAG_NAME(tagName) ELEKTRA_TAG (tagName)

#define ELEKTRA_TAG(tagName) ELEKTRA_TAG_##tagName

#define ELEKTRA_GET(typeName) elektraGet##typeName
#define ELEKTRA_GET_ARRAY_ELEMENT(typeName) elektraGet##typeName##ArrayElement
#define ELEKTRA_SET(typeName) elektraSet##typeName
#define ELEKTRA_SET_ARRAY_ELEMENT(typeName) elektraSet##typeName##ArrayElement

#define ELEKTRA_GET_SIGNATURE(cType, typeName) cType ELEKTRA_GET (typeName) (Elektra * elektra, const char * keyname)
#define ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE(cType, typeName)                                                                               \
	cType ELEKTRA_GET_ARRAY_ELEMENT (typeName) (Elektra * elektra, const char * keyname, kdb_long_long_t index)

#define ELEKTRA_GET_OUT_PTR_SIGNATURE(cType, typeName) void ELEKTRA_GET (typeName) (Elektra * elektra, const char * keyname, cType * result)
#define ELEKTRA_GET_ARRAY_ELEMENT_OUT_PTR_SIGNATURE(cType, typeName)                                                                       \
	void ELEKTRA_GET_ARRAY_ELEMENT (typeName) (Elektra * elektra, const char * keyname, kdb_long_long_t index, cType * result)

#define ELEKTRA_SET_SIGNATURE(cType, typeName)                                                                                             \
	void ELEKTRA_SET (typeName) (Elektra * elektra, const char * keyname, cType value, ElektraError ** error)
#define ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE(cType, typeName)                                                                               \
	void ELEKTRA_SET_ARRAY_ELEMENT (typeName) (Elektra * elektra, const char * keyname, kdb_long_long_t index, cType value,            \
						   ElektraError ** error)

#define ELEKTRA_KEY_TO(typeName) elektraKeyTo##typeName
#define ELEKTRA_KEY_TO_SIGNATURE(cType, typeName) int ELEKTRA_KEY_TO (typeName) (const Key * key, /*%& native_type %*/ *variable)

#define ELEKTRA_TO_STRING(typeName) elektra##typeName##ToString
#define ELEKTRA_TO_STRING_SIGNATURE(cType, typeName) char * ELEKTRA_TO_STRING (typeName) (/*%& native_type %*/ value)

// endregion Helpers for Tag Macros

/**
 * \defgroup highlevel High-level API
 * @{
 */
#ifdef __cplusplus
#define Key ckdb::Key
#define KeySet ckdb::KeySet
extern "C" {
#endif

typedef struct _Elektra Elektra;

// region Tag Macros
/**************************************
 *
 * Tag Macros
 *
 **************************************/


#define ELEKTRA_VALUE_TAG_STRUCT(cType, typeName)                                                                                          \
	ELEKTRA_TAG_TYPE (typeName)                                                                                                        \
	{                                                                                                                                  \
		const char * keyName;                                                                                                      \
		ELEKTRA_GET_BY_TAG_POINTER (get, cType, typeName);                                                                         \
		ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_POINTER (getArrayElement, cType, typeName);                                               \
		ELEKTRA_SET_BY_TAG_POINTER (set, cType, typeName);                                                                         \
		ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_POINTER (setArrayElement, cType, typeName);                                               \
	};

#define ELEKTRA_STRUCT_TAG_STRUCT(cType, typeName)                                                                                         \
	ELEKTRA_TAG_TYPE (typeName)                                                                                                        \
	{                                                                                                                                  \
		const char * keyName;                                                                                                      \
		ELEKTRA_GET_OUT_PTR_BY_TAG_POINTER (get, cType, typeName);                                                                 \
		ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_BY_TAG_POINTER (getArrayElement, cType, typeName);                                       \
		ELEKTRA_SET_BY_TAG_POINTER (set, cType, typeName);                                                                         \
		ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_POINTER (setArrayElement, cType, typeName);                                               \
	};

#define ELEKTRA_TAG_DECLARATIONS(cType, typeName) ELEKTRA_VALUE_TAG_DECLARATIONS (cType, typeName)

/**
 * Inserts the necessary declarations for a new Elektra Value Tag that can be used in combination with
 * the generic getter and setter macros.
 *
 * @param cType    The C-Type of the value described by the tag.
 * @param typeName The unique identifier of this type that can be used as part of a C identifier.
 */
#define ELEKTRA_VALUE_TAG_DECLARATIONS(cType, typeName)                                                                                    \
	ELEKTRA_VALUE_TAG_STRUCT (cType, typeName)                                                                                         \
                                                                                                                                           \
	ELEKTRA_GET_BY_TAG_SIGNATURE (cType, typeName);                                                                                    \
	ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (cType, typeName);                                                                      \
                                                                                                                                           \
	ELEKTRA_SET_BY_TAG_SIGNATURE (cType, typeName);                                                                                    \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (cType, typeName);

#define ELEKTRA_TAG_DEFINITIONS ELEKTRA_VALUE_TAG_DEFINITIONS

/**
 * Inserts the necessary definitions for an Elektra Value Tag declared with #ELEKTRA_TAG_DECLARATIONS
 *
 * @param cType           Exact same value as in #ELEKTRA_TAG_DECLARATIONS
 * @param typeName        Exact same value as in #ELEKTRA_TAG_DECLARATIONS
 * @param KDB_TYPE        The KDBType used in the type metadata of a Key.
 * @param VALUE_TO_STRING A function which converts a value of type @p cType into a string.
 *                        The string must be allocated with elektraMalloc(), so that it can be freed with elektraFree().
 * @param KEY_TO_VALUE    A function which takes a Key an pointer to a variable of type @p cType and tries to convert
 *                        the Key into @p cType. The function must return 1 on success and 0 otherwise.
 *                        The supermacro "macros/type_create_to_value.h" may be used to create such a function.
 */
#define ELEKTRA_VALUE_TAG_DEFINITIONS(cType, typeName, KDB_TYPE, VALUE_TO_STRING, KEY_TO_VALUE)                                            \
	ELEKTRA_SET_BY_TAG_SIGNATURE (cType, typeName)                                                                                     \
	{                                                                                                                                  \
		char * string = VALUE_TO_STRING (value);                                                                                   \
		if (string == 0)                                                                                                           \
		{                                                                                                                          \
			*error = elektraErrorConversionToString (KDB_TYPE, tag->keyName);                                                  \
			return;                                                                                                            \
		}                                                                                                                          \
		elektraSetRawString (elektra, tag->keyName, string, KDB_TYPE, error);                                                      \
		elektraFree (string);                                                                                                      \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (cType, typeName)                                                                       \
	{                                                                                                                                  \
		char * string = VALUE_TO_STRING (value);                                                                                   \
		if (string == 0)                                                                                                           \
		{                                                                                                                          \
			*error = elektraErrorConversionToString (KDB_TYPE, tag->keyName);                                                  \
			return;                                                                                                            \
		}                                                                                                                          \
		elektraSetRawStringArrayElement (elektra, tag->keyName, index, string, KDB_TYPE, error);                                   \
		elektraFree (string);                                                                                                      \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_GET_BY_TAG_SIGNATURE (cType, typeName)                                                                                     \
	{                                                                                                                                  \
		cType result;                                                                                                              \
		const Key * key = elektraFindKey (elektra, tag->keyName, KDB_TYPE);                                                        \
		if (!KEY_TO_VALUE (key, &result))                                                                                          \
		{                                                                                                                          \
			elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE, tag->keyName, keyString (key)));           \
			return ELEKTRA_CAST (cType, 0);                                                                                    \
		}                                                                                                                          \
		return result;                                                                                                             \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (cType, typeName)                                                                       \
	{                                                                                                                                  \
		cType result;                                                                                                              \
		const Key * key = elektraFindArrayElementKey (elektra, tag->keyName, index, KDB_TYPE);                                     \
		if (!KEY_TO_VALUE (key, &result))                                                                                          \
		{                                                                                                                          \
			elektraFatalError (elektra, elektraErrorConversionFromString (KDB_TYPE, tag->keyName, keyString (key)));           \
			return ELEKTRA_CAST (cType, 0);                                                                                    \
		}                                                                                                                          \
		return result;                                                                                                             \
	}

#define ELEKTRA_TAG_VALUE(tagName, keyname, typeName) ELEKTRA_VALUE_TAG_VALUE (tagName, keyname, typeName)

/**
 * Inserts a new static instance of an Elektra Value Tag.
 *
 * @param tagName  The name of the new Tag instance. Will be prefixed with `ELEKTRA_TAG_`.
 * @param keyname  The name of the key this Tag instance corresponds to.
 * @param typeName Exact same value as in #ELEKTRA_TAG_DECLARATIONS.
 *                 This value is used to identify which kind of tag should be created.
 */
#define ELEKTRA_VALUE_TAG_VALUE(tagName, keyname, typeName)                                                                                \
	static const ELEKTRA_TAG_TYPE (typeName) ELEKTRA_TAG_NAME (tagName) = {                                                            \
		(keyname),                                                                                                                 \
		ELEKTRA_GET_BY_TAG (typeName),                                                                                             \
		ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (typeName),                                                                               \
		ELEKTRA_SET_BY_TAG (typeName),                                                                                             \
		ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (typeName),                                                                               \
	};

/**
 * Inserts a new static instance of an Elektra Struct Tag.
 *
 * @param tagName  The name of the new Tag instance. Will be prefixed with `ELEKTRA_TAG_`.
 * @param keyname  The name of the key this Tag instance corresponds to.
 * @param typeName Exact same value as in #ELEKTRA_TAG_DECLARATIONS.
 *                 This value is used to identify which kind of tag should be created.
 */
#define ELEKTRA_STRUCT_TAG_VALUE(tagName, keyname, typeName)                                                                               \
	static const ELEKTRA_TAG_TYPE (typeName) ELEKTRA_TAG_NAME (tagName) = {                                                            \
		(keyname),                                                                                                                 \
		ELEKTRA_GET_BY_TAG (typeName),                                                                                             \
		ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (typeName),                                                                               \
		ELEKTRA_SET_BY_TAG (typeName),                                                                                             \
		ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (typeName),                                                                               \
	};

// endregion

// region Tags for built-in types
/**************************************
 *
 * Tags for built-in types
 *
 **************************************/

ELEKTRA_TAG_DECLARATIONS (const char *, String)
ELEKTRA_TAG_DECLARATIONS (kdb_boolean_t, Boolean)
ELEKTRA_TAG_DECLARATIONS (kdb_char_t, Char)
ELEKTRA_TAG_DECLARATIONS (kdb_octet_t, Octet)
ELEKTRA_TAG_DECLARATIONS (kdb_short_t, Short)
ELEKTRA_TAG_DECLARATIONS (kdb_unsigned_short_t, UnsignedShort)
ELEKTRA_TAG_DECLARATIONS (kdb_long_t, Long)
ELEKTRA_TAG_DECLARATIONS (kdb_unsigned_long_t, UnsignedLong)
ELEKTRA_TAG_DECLARATIONS (kdb_long_long_t, LongLong)
ELEKTRA_TAG_DECLARATIONS (kdb_unsigned_long_long_t, UnsignedLongLong)
ELEKTRA_TAG_DECLARATIONS (kdb_float_t, Float)
ELEKTRA_TAG_DECLARATIONS (kdb_double_t, Double)

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

ELEKTRA_TAG_DECLARATIONS (kdb_long_double_t, LongDouble)

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

// endregion Tags for built-in types

// region Basics
/**************************************
 *
 * Basics
 *
 **************************************/

Elektra * elektraOpen (const char * application, KeySet * defaults, ElektraError ** error);
void elektraClose (Elektra * elektra);

// endregion Basics

// region Error-Handling
/**************************************
 *
 * Error-Handling
 *
 **************************************/

void elektraFatalErrorHandler (Elektra * elektra, ElektraErrorHandler fatalErrorHandler);

// endregion

// region Helpers for code generation
/**************************************
 *
 * Helpers for code generation
 *
 **************************************/

Key * elektraFindKey (Elektra * elektra, const char * name, KDBType type);
Key * elektraFindArrayElementKey (Elektra * elektra, const char * name, kdb_long_long_t index, KDBType type);
void elektraFatalError (Elektra * elektra, ElektraError * fatalError);

// endregion Helpers for code generation

// region Getters
/**************************************
 *
 * Getters
 *
 **************************************/

const char * elektraGetRawString (Elektra * elektra, const char * name);
const char * elektraGetString (Elektra * elektra, const char * keyname);
kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * keyname);
kdb_char_t elektraGetChar (Elektra * elektra, const char * keyname);
kdb_octet_t elektraGetOctet (Elektra * elektra, const char * keyname);
kdb_short_t elektraGetShort (Elektra * elektra, const char * keyname);
kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * keyname);
kdb_long_t elektraGetLong (Elektra * elektra, const char * keyname);
kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * keyname);
kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * keyname);
kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * keyname);
kdb_float_t elektraGetFloat (Elektra * elektra, const char * keyname);
kdb_double_t elektraGetDouble (Elektra * elektra, const char * keyname);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * keyname);

#endif

// endregion Getters

// region Setters
/**************************************
 *
 * Setters
 *
 **************************************/

void elektraSetRawString (Elektra * elektra, const char * name, const char * value, KDBType type, ElektraError ** error);
void elektraSetString (Elektra * elektra, const char * keyName, const char * value, ElektraError ** error);
void elektraSetBoolean (Elektra * elektra, const char * keyname, kdb_boolean_t value, ElektraError ** error);
void elektraSetChar (Elektra * elektra, const char * keyname, kdb_char_t value, ElektraError ** error);
void elektraSetOctet (Elektra * elektra, const char * keyname, kdb_octet_t value, ElektraError ** error);
void elektraSetShort (Elektra * elektra, const char * keyname, kdb_short_t value, ElektraError ** error);
void elektraSetUnsignedShort (Elektra * elektra, const char * keyname, kdb_unsigned_short_t value, ElektraError ** error);
void elektraSetLong (Elektra * elektra, const char * keyname, kdb_long_t value, ElektraError ** error);
void elektraSetUnsignedLong (Elektra * elektra, const char * keyname, kdb_unsigned_long_t value, ElektraError ** error);
void elektraSetLongLong (Elektra * elektra, const char * keyname, kdb_long_long_t value, ElektraError ** error);
void elektraSetUnsignedLongLong (Elektra * elektra, const char * keyname, kdb_unsigned_long_long_t value, ElektraError ** error);
void elektraSetFloat (Elektra * elektra, const char * keyname, kdb_float_t value, ElektraError ** error);
void elektraSetDouble (Elektra * elektra, const char * keyname, kdb_double_t value, ElektraError ** error);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

void elektraSetLongDouble (Elektra * elektra, const char * keyname, kdb_long_double_t value, ElektraError ** error);

#endif

// endregion Setters

// region Array-Helpers
/**************************************
 *
 * Array-Helpers
 *
 **************************************/

kdb_long_long_t elektraArraySize (Elektra * elektra, const char * keyName);

// endregion Array-Helpers

// region Array-Getters
/**************************************
 *
 * Array-Getters
 *
 **************************************/

const char * elektraGetRawStringArrayElement (Elektra * elektra, const char * name, kdb_long_long_t index);
const char * elektraGetStringArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_boolean_t elektraGetBooleanArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_char_t elektraGetCharArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_octet_t elektraGetOctetArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_short_t elektraGetShortArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_unsigned_short_t elektraGetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_long_t elektraGetLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_unsigned_long_t elektraGetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_long_long_t elektraGetLongLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_unsigned_long_long_t elektraGetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_float_t elektraGetFloatArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);
kdb_double_t elektraGetDoubleArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

kdb_long_double_t elektraGetLongDoubleArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index);

#endif

// endregion Array-Getters

// region Array-Setters
/**************************************
 *
 * Array-Setters
 *
 **************************************/

void elektraSetRawStringArrayElement (Elektra * elektra, const char * name, kdb_long_long_t index, const char * value, KDBType type,
				      ElektraError ** error);
void elektraSetStringArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, const char * value,
				   ElektraError ** error);
void elektraSetBooleanArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_boolean_t value,
				    ElektraError ** error);
void elektraSetCharArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_char_t value, ElektraError ** error);
void elektraSetOctetArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_octet_t value, ElektraError ** error);
void elektraSetShortArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_short_t value, ElektraError ** error);
void elektraSetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_unsigned_short_t value,
					  ElektraError ** error);
void elektraSetLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_long_t value, ElektraError ** error);
void elektraSetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_unsigned_long_t value,
					 ElektraError ** error);
void elektraSetLongLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_long_long_t value,
				     ElektraError ** error);
void elektraSetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_unsigned_long_long_t value,
					     ElektraError ** error);
void elektraSetFloatArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_float_t value, ElektraError ** error);
void elektraSetDoubleArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_double_t value,
				   ElektraError ** error);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

void elektraSetLongDoubleArrayElement (Elektra * elektra, const char * keyname, kdb_long_long_t index, kdb_long_double_t value,
				       ElektraError ** error);

#endif

// endregion Array-Setters

// region Type information
/**************************************
 *
 * Type information
 *
 **************************************/

KDBType elektraGetType (Elektra * elektra, const char * keyname);
KDBType elektraGetArrayElementType (Elektra * elektra, const char * name, kdb_long_long_t index);

// endregion

// region Generic Getters and Setters
/**************************************
 *
 * Generic Getters and Setters
 *
 **************************************/

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The tag to look up.
 * @return The value stored at the given key and index.
 */
#define elektraGet(elektra, tag) ((tag).get (elektra, &(tag)))

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The tag to look up.
 * @param result Points to the struct into which results will be stored.
 */
#define elektraGetStruct(elektra, tag, result) ((tag).get (elektra, &(tag), result))

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The tag to look up.
 * @param index The array index of the desired element, starting with 0.
 * @return The value stored at the given key and index.
 */
#define elektraGetArrayElement(elektra, tag, index) ((tag).getArrayElement (elektra, &(tag), index))

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The tag to look up.
 * @param index The array index of the desired element, starting with 0.
 * @param result Points to the struct into which results will be stored.
 */
#define elektraGetStructArrayElement(elektra, tag, index, result) ((tag).getArrayElement (elektra, &(tag), index, result))

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The codegenerated Tag to write to.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 */
#define elektraSet(elektra, tag, value, error) ((tag).set (elektra, &(tag), value, error))

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The code-generated tag to write to.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 * @param ... Strings to replace dynamic parts (_, #) of keyname.
 */
#define elektraSetArrayElement(elektra, tag, index, value, error) ((tag).setArrayElement (elektra, &(tag), index, value, error))

// endregion Generic Getters and Setters

#ifdef __cplusplus
}
#undef Key
#undef KeySet
#endif

/**
 * @}
 */

#endif // ELEKTRA_H

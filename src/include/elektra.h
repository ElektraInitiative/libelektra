/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_H
#define ELEKTRA_H

#include "elektra_conversion.h"
#include "elektra_error.h"
#include "kdb.h"
#include "kdbtypes.h"
#include <string.h>

/**
 * \defgroup highlevel High-level API
 * @{
 */

typedef struct _Elektra Elektra;

#define ELEKTRA_TAG(typeName) struct Elektra##typeName##Tag

#define ELEKTRA_SET_BY_TAG(typeName) __elektraSet##typeName##ByTag
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(typeName) __elektraSet##typeName##ArrayElementByTag

#define ELEKTRA_GET_BY_TAG(typeName) __elektraGet##typeName##ByTag
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(typeName) __elektraGet##typeName##ArrayElementByTag


#define ELEKTRA_GET_BY_TAG_PARAMS(typeName) (Elektra * elektra, const ELEKTRA_TAG (typeName) * tag)
#define ELEKTRA_GET_BY_TAG_SIGNATURE(cType, typeName) cType ELEKTRA_GET_BY_TAG (typeName) ELEKTRA_GET_BY_TAG_PARAMS (typeName)

#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_PARAMS(typeName) (Elektra * elektra, const ELEKTRA_TAG (typeName) * tag, size_t index)
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(cType, typeName)                                                                        \
	cType ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (typeName) ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_PARAMS (typeName)


#define ELEKTRA_SET_BY_TAG_PARAMS(cType, typeName)                                                                                         \
	(Elektra * elektra, const ELEKTRA_TAG (typeName) * tag, cType value, ElektraError * *error)
#define ELEKTRA_SET_BY_TAG_SIGNATURE(cType, typeName) void ELEKTRA_SET_BY_TAG (typeName) ELEKTRA_SET_BY_TAG_PARAMS (cType, typeName)

#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_PARAMS(cType, typeName)                                                                           \
	(Elektra * elektra, const ELEKTRA_TAG (typeName) * tag, size_t index, cType value, ElektraError * *error)
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(cType, typeName)                                                                        \
	void ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (typeName) ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_PARAMS (cType, typeName)

#define ELEKTRA_TAG_DECLARATIONS(cType, typeName)                                                                                          \
	ELEKTRA_TAG (typeName)                                                                                                             \
	{                                                                                                                                  \
		char * keyName;                                                                                                            \
		cType (*get) ELEKTRA_GET_BY_TAG_PARAMS (typeName);                                                                         \
		cType (*getArrayElement) ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_PARAMS (typeName);                                               \
		void(*set) ELEKTRA_SET_BY_TAG_PARAMS (cType, typeName);                                                                    \
		void(*setArrayElement) ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_PARAMS (cType, typeName);                                          \
	};                                                                                                                                 \
                                                                                                                                           \
	ELEKTRA_GET_BY_TAG_SIGNATURE (cType, typeName);                                                                                    \
	ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (cType, typeName);                                                                      \
                                                                                                                                           \
	ELEKTRA_SET_BY_TAG_SIGNATURE (cType, typeName);                                                                                    \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (cType, typeName);


#define ELEKTRA_TAG_DEFINITIONS(cType, typeName, KDB_TYPE, TO_STRING, FROM_STRING)                                                         \
	ELEKTRA_SET_BY_TAG_SIGNATURE (cType, typeName)                                                                                     \
	{                                                                                                                                  \
		elektraSetValue (elektra, tag->keyName, TO_STRING (value), KDB_TYPE, error);                                               \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (cType, typeName)                                                                       \
	{                                                                                                                                  \
		elektraSetArrayElementValue (elektra, tag->keyName, index, TO_STRING (value), KDB_TYPE, error);                            \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_GET_BY_TAG_SIGNATURE (cType, typeName)                                                                                     \
	{                                                                                                                                  \
		return FROM_STRING (elektraGetValue (elektra, tag->keyName, KDB_TYPE));                                                    \
	}                                                                                                                                  \
                                                                                                                                           \
	ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE (cType, typeName)                                                                       \
	{                                                                                                                                  \
		return FROM_STRING (elektraGetArrayElementValue (elektra, tag->keyName, index, KDB_TYPE));                                 \
	}

#define ELEKTRA_TAG_NAME(tagName) ELEKTRA_TAG_##tagName
#define ELEKTRA_TAG_VALUE(tagName, _keyName, typeName)                                                                                     \
	static const ELEKTRA_TAG (typeName) ELEKTRA_TAG_NAME (tagName) = {                                                                 \
		.keyName = (_keyName),                                                                                                     \
		.get = ELEKTRA_GET_BY_TAG (typeName),                                                                                      \
		.getArrayElement = ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG (typeName),                                                            \
		.set = ELEKTRA_SET_BY_TAG (typeName),                                                                                      \
		.setArrayElement = ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG (typeName),                                                            \
	};

// Elektra built-in types

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
ELEKTRA_TAG_DECLARATIONS (kdb_long_double_t, LongDouble)
ELEKTRA_TAG_DECLARATIONS (int, Enum)

typedef const char * KDBType;

extern KDBType KDB_TYPE_STRING;
extern KDBType KDB_TYPE_BOOLEAN;
extern KDBType KDB_TYPE_CHAR;
extern KDBType KDB_TYPE_OCTET;
extern KDBType KDB_TYPE_SHORT;
extern KDBType KDB_TYPE_UNSIGNED_SHORT;
extern KDBType KDB_TYPE_LONG;
extern KDBType KDB_TYPE_UNSIGNED_LONG;
extern KDBType KDB_TYPE_LONG_LONG;
extern KDBType KDB_TYPE_UNSIGNED_LONG_LONG;
extern KDBType KDB_TYPE_FLOAT;
extern KDBType KDB_TYPE_LONG_DOUBLE;
extern KDBType KDB_TYPE_DOUBLE;
extern KDBType KDB_TYPE_ENUM;

/**
 * Initializes a new Elektra instance.
 * @param application The parent key for your application.
 * @param defaults A KeySet containing default values. Passing NULL means "no default values".
 * @return An Elektra instance initialized with the application.
 */
Elektra * elektraOpen (const char * application, KeySet * defaults, ElektraError ** error);

/**
 * Releases all ressources used by the given elektra instance. The elektra instance must not be used anymore after calling this.
 * @param elektra An Elektra instance.
 */
void elektraClose (Elektra * elektra);

size_t elektraArraySize (Elektra * elektra, const char * keyName);

void elektraSetValue (Elektra * elektra, const char * name, const char * value, KDBType type, ElektraError ** error);
const char * elektraGetValue (Elektra * elektra, const char * name, KDBType type);

void elektraSetArrayElementValue (Elektra * elektra, const char * name, size_t index, const char * value, KDBType type,
				  ElektraError ** error);
const char * elektraGetArrayElementValue (Elektra * elektra, const char * name, size_t index, KDBType type);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyname The (relative) keyname to write to. The keyname is appended to the parent key.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 */
void elektraSetString (Elektra * elektra, const char * keyName, const char * value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetBoolean (Elektra * elektra, const char * keyname, kdb_boolean_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetChar (Elektra * elektra, const char * keyname, kdb_char_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetOctet (Elektra * elektra, const char * keyname, kdb_octet_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetShort (Elektra * elektra, const char * keyname, kdb_short_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetUnsignedShort (Elektra * elektra, const char * keyname, kdb_unsigned_short_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetLong (Elektra * elektra, const char * keyname, kdb_long_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetUnsignedLong (Elektra * elektra, const char * keyname, kdb_unsigned_long_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetLongLong (Elektra * elektra, const char * keyname, kdb_long_long_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetUnsignedLongLong (Elektra * elektra, const char * keyname, kdb_unsigned_long_long_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetFloat (Elektra * elektra, const char * keyname, kdb_float_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetDouble (Elektra * elektra, const char * keyname, kdb_double_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetLongDouble (Elektra * elektra, const char * keyname, kdb_long_double_t value, ElektraError ** error);

/**
 * @copydoc elektraSetString
 */
void elektraSetEnum (Elektra * elektra, char * name, int value, ElektraError ** error);


// Getters


/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyname The (relative) keyname to look up. The keyname is appended to the parent key.
 * @return The value stored at the given key and index.
 */
const char * elektraGetString (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_char_t elektraGetChar (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_octet_t elektraGetOctet (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_short_t elektraGetShort (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_long_t elektraGetLong (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_float_t elektraGetFloat (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_double_t elektraGetDouble (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * keyname);

/**
 * @copydoc elektraGetString
 */
int elektraGetEnumInt (Elektra * elektra, char * keyName);

#define elektraGetEnum(elektra, keyname, enumType) (enumType) elektraGetEnumInt (elektra, keyname)


/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyname The keyname (or a codegenerated Tag) to write to. The keyname is appended to the parent key.
 * @param value The new value.
 * @param value Pass a reference to an ElektraError pointer.
 */
void elektraSetStringArrayElement (Elektra * elektra, const char * keyname, size_t index, const char * value, ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetBooleanArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_boolean_t value,
					   ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetCharArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_char_t value, ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetOctetArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_octet_t value, ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetShortArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_short_t value, ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_short_t value,
						 ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_t value, ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_long_t value,
						ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_long_t value,
					    ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_unsigned_long_long_t value,
						    ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetFloatArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_float_t value, ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_double_t value, ElektraError ** error);

/**
 * @copydoc elektraSetStringArrayElement
 */
void elektraSetLongDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index, kdb_long_double_t value,
					      ElektraError ** error);

void elektraSetEnumArrayElement (Elektra * elektra, char * name, size_t index, int value, ElektraError ** error);

// Array-Getters

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyName The keyname (or a codegenerated Tag) to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0. \
 * @return The value stored at the given key and index.
 */
const char * elektraGetStringArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_boolean_t elektraGetBooleanArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_char_t elektraGetCharArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_octet_t elektraGetOctetArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_short_t elektraGetShortArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_unsigned_short_t elektraGetUnsignedShortArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_long_t elektraGetLongArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_unsigned_long_t elektraGetUnsignedLongArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_long_long_t elektraGetLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_unsigned_long_long_t elektraGetUnsignedLongLongArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_float_t elektraGetFloatArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_double_t elektraGetDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index);

/**
 * @copydoc elektraGetStringArrayElement
 */
kdb_long_double_t elektraGetLongDoubleArrayElement (Elektra * elektra, const char * keyname, size_t index);

int elektraGetEnumArrayElementInt (Elektra * elektra, char * keyName, int index);

#define elektraGetEnumArrayElement(elektra, keyname, index, enumType) (enumType) elektraGetEnumArrayElementInt (elektra, keyname, index)

// Generic Setters and Getters

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param tag The codegenerated Tag to write to.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 */
#define elektraSet(elektra, tag, value, error) ((tag).set(elektra, &(tag), value, error))

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keynameOrTag The keyname (or a codegenerated Tag) to write to. The keyname is appended to the parent key.
 * @param value The new value.
 * @param error Pass a reference to an ElektraError pointer.
 */
#define elektraSetArrayElement(elektra, tag, index, value, error) ((tag).setArrayElement(elektra, &(tag), index, value, error))

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 * @return The value stored at the given key and index.
 */
#define elektraGet(elektra, tag) ((tag).get(elektra, &(tag)))

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param keyName The keyname (or a codegenerated Tag) to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0. \
 * @return The value stored at the given key and index.
 */
#define elektraGetArrayElement(elektra, tag, index) ((tag).getArrayElement(elektra, &(tag), index))
/**
 * @}
 */

#endif // ELEKTRA_H

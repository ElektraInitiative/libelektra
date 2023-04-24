/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_H
#define ELEKTRA_H

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/highlevel/errors.h>
#include <elektra/highlevel/types.h>
#include <elektra/type/types.h>
#include <elektra/macros/symver.h>
// region Helpers for Code Generation
#define ELEKTRA_GET(typeName) ELEKTRA_CONCAT (elektraGet, typeName)
#define ELEKTRA_GET_ARRAY_ELEMENT(typeName) ELEKTRA_CONCAT (ELEKTRA_CONCAT (elektraGet, typeName), ArrayElement)
#define ELEKTRA_SET(typeName) ELEKTRA_CONCAT (elektraSet, typeName)
#define ELEKTRA_SET_ARRAY_ELEMENT(typeName) ELEKTRA_CONCAT (ELEKTRA_CONCAT (elektraSet, typeName), ArrayElement)

#define ELEKTRA_GET_SIGNATURE(cType, typeName) cType ELEKTRA_GET (typeName) (Elektra * elektra, const char * keyname)
#define ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE(cType, typeName)                                                                               \
	cType ELEKTRA_GET_ARRAY_ELEMENT (typeName) (Elektra * elektra, const char * keyname, kdb_long_long_t index)

#define ELEKTRA_GET_OUT_PTR_SIGNATURE(cType, typeName) void ELEKTRA_GET (typeName) (Elektra * elektra, const char * keyname, cType * result)
#define ELEKTRA_GET_OUT_PTR_ARRAY_ELEMENT_SIGNATURE(cType, typeName)                                                                       \
	void ELEKTRA_GET_ARRAY_ELEMENT (typeName) (Elektra * elektra, const char * keyname, kdb_long_long_t index, cType * result)

#define ELEKTRA_SET_SIGNATURE(cType, typeName)                                                                                             \
	void ELEKTRA_SET (typeName) (Elektra * elektra, const char * keyname, cType value, ElektraError ** error)
#define ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE(cType, typeName)                                                                               \
	void ELEKTRA_SET_ARRAY_ELEMENT (typeName) (Elektra * elektra, const char * keyname, kdb_long_long_t index, cType value,            \
						   ElektraError ** error)

#define ELEKTRA_KEY_TO(typeName) ELEKTRA_CONCAT (elektraKeyTo, typeName)
#define ELEKTRA_KEY_TO_SIGNATURE(cType, typeName) int ELEKTRA_KEY_TO (typeName) (const Key * key, cType * variable)

#define ELEKTRA_TO_STRING(typeName) ELEKTRA_CONCAT (ELEKTRA_CONCAT (elektra, typeName), ToString)
#define ELEKTRA_TO_STRING_SIGNATURE(cType, typeName) char * ELEKTRA_TO_STRING (typeName) (cType value)

// endregion Helpers for Code Generation

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

// region Basics
/**************************************
 *
 * Basics
 *
 **************************************/

Elektra * ELEKTRA_SYMVER (elektraOpen, v1) (const char * application, KeySet * defaults, ElektraError ** error);

Elektra * elektraOpen (const char * application, KeySet * defaults, KeySet * contract, ElektraError ** error);
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

const char * elektraFindReference (Elektra * elektra, const char * name);
const char * elektraFindReferenceArrayElement (Elektra * elektra, const char * name, kdb_long_long_t index);

Key * elektraHelpKey (Elektra * elektra);

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

#ifdef __cplusplus
}
#undef Key
#undef KeySet
#endif

/**
 * @}
 */

#endif // ELEKTRA_H

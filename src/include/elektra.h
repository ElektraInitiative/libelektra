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

#define ELEKTRA_SET(typeName) elektraSet##typeName
#define ELEKTRA_SET_BY_TAG(typeName) elektraSet##typeName##ByTag
#define ELEKTRA_SET_ARRAY_ELEMENT(typeName) elektraSet##typeName##ArrayElement
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(typeName) elektraSet##typeName##ArrayElementByTag

#define ELEKTRA_GET(typeName) elektraGet##typeName
#define ELEKTRA_GET_BY_TAG(typeName) elektraGet##typeName##ByTag
#define ELEKTRA_GET_ARRAY_ELEMENT(typeName) elektraGet##typeName##ArrayElement
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(typeName) elektraGet##typeName##ArrayElementByTag

#define ELEKTRA_SET_SIGNATURE(Type, typeName) void ELEKTRA_SET(typeName)(Elektra * elektra, const char * keyName, Type value, ElektraError ** error)
#define ELEKTRA_SET_BY_TAG_SIGNATURE(Type, typeName) void ELEKTRA_SET_BY_TAG(typeName)(Elektra * elektra, ELEKTRA_TAG(typeName) tag, Type value, ElektraError ** error)
#define ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE(Type, typeName) void ELEKTRA_SET_ARRAY_ELEMENT(typeName)(Elektra * elektra, const char * keyName, Type value, size_t index, ElektraError ** error)
#define ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName) void ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(typeName)(Elektra * elektra, ELEKTRA_TAG(typeName) tag, Type value, size_t index, ElektraError ** error)

#define ELEKTRA_GET_SIGNATURE(Type, typeName) Type ELEKTRA_GET(typeName)(Elektra * elektra, const char * keyName)
#define ELEKTRA_GET_BY_TAG_SIGNATURE(Type, typeName) Type ELEKTRA_GET_BY_TAG(typeName)(Elektra * elektra, ELEKTRA_TAG(typeName) tag)
#define ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE(Type, typeName) Type ELEKTRA_GET_ARRAY_ELEMENT(typeName)(Elektra * elektra, const char * keyName, size_t index)
#define ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName) Type ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(typeName)(Elektra * elektra, ELEKTRA_TAG(typeName) tag, size_t index)

#define ELEKTRA_DECLARATION(Type, typeName) \
    typedef struct { \
        char * keyName; \
    } ELEKTRA_TAG(typeName); \
\
    ELEKTRA_SET_SIGNATURE(Type, typeName); \
    ELEKTRA_SET_BY_TAG_SIGNATURE(Type, typeName); \
    ELEKTRA_SET_ARRAY_ELEMENT_SIGNATURE(Type, typeName); \
    ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName); \
\
    ELEKTRA_GET_SIGNATURE(Type, typeName); \
    ELEKTRA_GET_BY_TAG_SIGNATURE(Type, typeName); \
    ELEKTRA_GET_ARRAY_ELEMENT_SIGNATURE(Type, typeName); \
    ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG_SIGNATURE(Type, typeName);


#define ELEKTRA_TYPES(X) \
    X(const char *, String) \
    X(kdb_boolean_t, Boolean) \
    X(kdb_char_t, Char) \
    X(kdb_octet_t, Octet) \
    X(kdb_short_t, Short) \
    X(kdb_unsigned_short_t, UnsignedShort) \
    X(kdb_long_t, Long) \
    X(kdb_unsigned_long_t, UnsignedLong) \
    X(kdb_long_long_t, LongLong) \
    X(kdb_unsigned_long_long_t, UnsignedLongLong) \
    X(kdb_float_t, Float) \
    X(kdb_double_t, Double) \
    X(kdb_long_double_t, LongDouble)

ELEKTRA_TYPES(ELEKTRA_DECLARATION)
#undef ELEKTRA_DECLARATION


#define ELEKTRA_GENERIC_SET_ENTRY(typeName) \
    ELEKTRA_TAG(typeName): ELEKTRA_SET_BY_TAG(typeName),

#define ELEKTRA_GENERIC_SET_ARRAY_ELEMENT_ENTRY(typeName) \
    ELEKTRA_TAG(typeName): ELEKTRA_SET_BY_TAG(typeName),

#define ELEKTRA_GENERIC_GET_ENTRY(typeName) \
    ELEKTRA_TAG(typeName): ELEKTRA_GET_BY_TAG(typeName),

#define ELEKTRA_GENERIC_GET_ARRAY_ELEMENT_ENTRY(typeName) \
    ELEKTRA_TAG(typeName): ELEKTRA_GET_BY_TAG(typeName),


#define ELEKTRA_TAG_NAMES_EXCEPT_STRING(X) \
    X(Boolean) \
    X(Char) \
    X(Octet) \
    X(Short) \
    X(UnsignedShort) \
    X(Long) \
    X(UnsignedLong) \
    X(LongLong) \
    X(UnsignedLongLong) \
    X(Float) \
    X(Double) \
    X(LongDouble)


#define elektraSet(ELEKTRA, TAG, VALUE, ERROR) \
    _Generic((TAG), \
      ELEKTRA_TAG_NAMES_EXCEPT_STRING(ELEKTRA_GENERIC_SET_ENTRY) \
      ELEKTRA_TAG(String): ELEKTRA_SET_BY_TAG(String))(ELEKTRA, TAG, VALUE, ERROR)

#define elektraSetArrayElement(ELEKTRA, TAG, VALUE, INDEX, ERROR) \
    _Generic((TAG), \
      ELEKTRA_TAG_NAMES_EXCEPT_STRING(ELEKTRA_GENERIC_SET_ARRAY_ELEMENT_ENTRY) \
      ELEKTRA_TAG(String): ELEKTRA_SET_ARRAY_ELEMENT_BY_TAG(String))(ELEKTRA, TAG, VALUE, INDEX, ERROR)


#define elektraGet(ELEKTRA, TAG) \
  _Generic((TAG), \
      ELEKTRA_TAG_NAMES_EXCEPT_STRING(ELEKTRA_GENERIC_GET_ENTRY) \
      ELEKTRA_TAG(String): ELEKTRA_GET_BY_TAG(String))(ELEKTRA, TAG)

#define elektraGetArrayElement(ELEKTRA, TAG, INDEX) \
  _Generic((TAG), \
      ELEKTRA_TAG_NAMES_EXCEPT_STRING(ELEKTRA_GENERIC_GET_ARRAY_ELEMENT_ENTRY) \
      ELEKTRA_TAG(String): ELEKTRA_GET_ARRAY_ELEMENT_BY_TAG(String))(ELEKTRA, TAG, INDEX)


Elektra * elektraOpen (const char * application, KeySet * defaults, ElektraError ** error);
void elektraClose (Elektra * elektra);
size_t elektraArraySize (Elektra * elektra, const char * keyName);

#endif // ELEKTRA_H

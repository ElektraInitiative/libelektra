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

Elektra * elektraOpen (const char * application, KeySet * defaults, ElektraError ** error);
void elektraClose (Elektra * elektra);

void elektraSetString (Elektra * elektra, const char * name, const char * value, ElektraError ** error);
void elektraSetBoolean (Elektra * elektra, const char * name, kdb_boolean_t value, ElektraError ** error);
void elektraSetChar (Elektra * elektra, const char * name, kdb_char_t value, ElektraError ** error);
void elektraSetOctet (Elektra * elektra, const char * name, kdb_octet_t value, ElektraError ** error);
void elektraSetShort (Elektra * elektra, const char * name, kdb_short_t value, ElektraError ** error);
void elektraSetUnsignedShort (Elektra * elektra, const char * name, kdb_unsigned_short_t value, ElektraError ** error);
void elektraSetLong (Elektra * elektra, const char * name, kdb_long_t value, ElektraError ** error);
void elektraSetUnsignedLong (Elektra * elektra, const char * name, kdb_unsigned_long_t value, ElektraError ** error);
void elektraSetLongLong (Elektra * elektra, const char * name, kdb_long_long_t value, ElektraError ** error);
void elektraSetUnsignedLongLong (Elektra * elektra, const char * name, kdb_unsigned_long_long_t value, ElektraError ** error);
void elektraSetFloat (Elektra * elektra, const char * name, kdb_float_t value, ElektraError ** error);
void elektraSetDouble (Elektra * elektra, const char * name, kdb_double_t value, ElektraError ** error);
void elektraSetLongDouble (Elektra * elektra, const char * name, kdb_long_double_t value, ElektraError ** error);

void elektraSetStringArrayElement (Elektra * elektra, const char * name, const char * value, size_t index, ElektraError ** error);
void elektraSetBooleanArrayElement (Elektra * elektra, const char * name, kdb_boolean_t value, size_t index, ElektraError ** error);
void elektraSetCharArrayElement (Elektra * elektra, const char * name, kdb_char_t value, size_t index, ElektraError ** error);
void elektraSetOctetArrayElement (Elektra * elektra, const char * name, kdb_octet_t value, size_t index, ElektraError ** error);
void elektraSetShortArrayElement (Elektra * elektra, const char * name, kdb_short_t value, size_t index, ElektraError ** error);
void elektraSetUnsignedShortArrayElement (Elektra * elektra, const char * name, kdb_unsigned_short_t value, size_t index,
					  ElektraError ** error);
void elektraSetLongArrayElement (Elektra * elektra, const char * name, kdb_long_t value, size_t index, ElektraError ** error);
void elektraSetUnsignedLongArrayElement (Elektra * elektra, const char * name, kdb_unsigned_long_t value, size_t index,
					 ElektraError ** error);
void elektraSetLongLongArrayElement (Elektra * elektra, const char * name, kdb_long_long_t value, size_t index, ElektraError ** error);
void elektraSetUnsignedLongLongArrayElement (Elektra * elektra, const char * name, kdb_unsigned_long_long_t value, size_t index,
					     ElektraError ** error);
void elektraSetFloatArrayElement (Elektra * elektra, const char * name, kdb_float_t value, size_t index, ElektraError ** error);
void elektraSetDoubleArrayElement (Elektra * elektra, const char * name, kdb_double_t value, size_t index, ElektraError ** error);
void elektraSetLongDoubleArrayElement (Elektra * elektra, const char * name, kdb_long_double_t value, size_t index, ElektraError ** error);

const char * elektraGetString (Elektra * elektra, const char * name);
kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * name);
kdb_char_t elektraGetChar (Elektra * elektra, const char * name);
kdb_octet_t elektraGetOctet (Elektra * elektra, const char * name);
kdb_short_t elektraGetShort (Elektra * elektra, const char * name);
kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * name);
kdb_long_t elektraGetLong (Elektra * elektra, const char * name);
kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * name);
kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * name);
kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * name);
kdb_float_t elektraGetFloat (Elektra * elektra, const char * name);
kdb_double_t elektraGetDouble (Elektra * elektra, const char * name);
kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * name);
size_t elektraArraySize (Elektra * elektra, const char * name);

const char * elektraGetStringArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_boolean_t elektraGetBooleanArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_char_t elektraGetCharArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_octet_t elektraGetOctetArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_short_t elektraGetShortArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_unsigned_short_t elektraGetUnsignedShortArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_long_t elektraGetLongArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_unsigned_long_t elektraGetUnsignedLongArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_long_long_t elektraGetLongLongArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_unsigned_long_long_t elektraGetUnsignedLongLongArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_float_t elektraGetFloatArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_double_t elektraGetDoubleArrayElement (Elektra * elektra, const char * name, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
kdb_long_double_t elektraGetLongDoubleArrayElement (Elektra * elektra, const char * name, size_t index);

#endif // ELEKTRA_H

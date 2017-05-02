/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_H
#define ELEKTRA_H

#include "kdb.h"
#include "kdbtypes.h"
#include "elektra_error.h"

typedef struct _Elektra Elektra;

Elektra * elektraOpen (const char * application, ElektraError ** error);
void elektraClose (Elektra * elektra);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetString (Elektra * elektra, const char * name, const char * value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetBoolean (Elektra * elektra, const char * name, kdb_boolean_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetChar (Elektra * elektra, const char * name, kdb_char_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetOctet (Elektra * elektra, const char * name, kdb_octet_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetShort (Elektra * elektra, const char * name, kdb_short_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetUnsignedShort (Elektra * elektra, const char * name, kdb_unsigned_short_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetLong (Elektra * elektra, const char * name, kdb_long_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetUnsignedLong (Elektra * elektra, const char * name, kdb_unsigned_long_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetLongLong (Elektra * elektra, const char * name, kdb_long_long_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetUnsignedLongLong (Elektra * elektra, const char * name, kdb_unsigned_long_long_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetFloat (Elektra * elektra, const char * name, kdb_float_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetDouble (Elektra * elektra, const char * name, kdb_double_t value);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 */
void elektraSetLongDouble (Elektra * elektra, const char * name, kdb_long_double_t value);


/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetStringArrayElement (Elektra * elektra, const char * name, const char * value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetBooleanArrayElement (Elektra * elektra, const char * name, kdb_boolean_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetCharArrayElement (Elektra * elektra, const char * name, kdb_char_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetOctetArrayElement (Elektra * elektra, const char * name, kdb_octet_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetShortArrayElement (Elektra * elektra, const char * name, kdb_short_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetUnsignedShortArrayElement (Elektra * elektra, const char * name, kdb_unsigned_short_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetLongArrayElement (Elektra * elektra, const char * name, kdb_long_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetUnsignedLongArrayElement (Elektra * elektra, const char * name, kdb_unsigned_long_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetLongLongArrayElement (Elektra * elektra, const char * name, kdb_long_long_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetUnsignedLongLongArrayElement (Elektra * elektra, const char * name, kdb_unsigned_long_long_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetFloatArrayElement (Elektra * elektra, const char * name, kdb_float_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetDoubleArrayElement (Elektra * elektra, const char * name, kdb_double_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param value The new value.
 * @param index The array index of the desired element, starting with 0.
 */
void elektraSetLongDoubleArrayElement (Elektra * elektra, const char * name, kdb_long_double_t value, size_t index);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
const char * elektraGetString (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_char_t elektraGetChar (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_octet_t elektraGetOctet (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_short_t elektraGetShort (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_long_t elektraGetLong (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_float_t elektraGetFloat (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_double_t elektraGetDouble (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @returns The number of elements in the array.
 */
size_t elektraArraySize (Elektra * elektra, const char * name);

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
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
kdb_unsigned_short_t elektraGetUnsignedShortArrayElement  (Elektra * elektra, const char * name, size_t index);

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

#endif //ELEKTRA_H

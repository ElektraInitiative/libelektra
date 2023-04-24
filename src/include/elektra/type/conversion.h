/**
 * @file
 *
 * @brief Elektra conversion.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_TYPE_CONVERSION_H
#define ELEKTRA_TYPE_CONVERSION_H

int elektraKeyToString (const Key * key, const char ** variable);
int elektraKeyToBoolean (const Key * key, kdb_boolean_t * variable);
int elektraKeyToChar (const Key * key, kdb_char_t * variable);
int elektraKeyToOctet (const Key * key, kdb_octet_t * variable);
int elektraKeyToShort (const Key * key, kdb_short_t * variable);
int elektraKeyToUnsignedShort (const Key * key, kdb_unsigned_short_t * variable);
int elektraKeyToLong (const Key * key, kdb_long_t * variable);
int elektraKeyToUnsignedLong (const Key * key, kdb_unsigned_long_t * variable);
int elektraKeyToLongLong (const Key * key, kdb_long_long_t * variable);
int elektraKeyToUnsignedLongLong (const Key * key, kdb_unsigned_long_long_t * variable);
int elektraKeyToFloat (const Key * key, kdb_float_t * variable);
int elektraKeyToDouble (const Key * key, kdb_double_t * variable);

char * elektraBooleanToString (kdb_boolean_t value);
char * elektraCharToString (kdb_char_t value);
char * elektraOctetToString (kdb_octet_t value);
char * elektraShortToString (kdb_short_t value);
char * elektraUnsignedShortToString (kdb_unsigned_short_t value);
char * elektraLongToString (kdb_long_t value);
char * elektraUnsignedLongToString (kdb_unsigned_long_t value);
char * elektraLongLongToString (kdb_long_long_t value);
char * elektraUnsignedLongLongToString (kdb_unsigned_long_long_t value);
char * elektraFloatToString (kdb_float_t value);
char * elektraDoubleToString (kdb_double_t value);

#ifdef ELEKTRA_HAVE_KDB_LONG_DOUBLE

int elektraKeyToLongDouble (const Key * key, kdb_long_double_t * variable);

char * elektraLongDoubleToString (kdb_long_double_t value);

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

#endif // ELEKTRA_TYPE_CONVERSION_H

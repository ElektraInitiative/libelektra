#ifndef KDBEASE_H
#define KDBEASE_H

#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <elektra/type/types.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

int elektraKsFilter (KeySet * result, KeySet * input, int (*filter) (const Key * k, void * argument), void * argument);
int elektraKsToMemArray (KeySet * ks, Key ** buffer);

int elektraArrayIncName (Key * key);
int elektraArrayDecName (Key * key);

int elektraArrayValidateName (const Key * key);
int elektraArrayValidateBaseNameString (const char * baseName);

const char * elektraKeyGetRelativeName (Key const * cur, Key const * parentKey);

KeySet * elektraArrayGet (const Key * arrayParent, KeySet * keys);
Key * elektraArrayGetNextKey (KeySet * arrayKeys);

int elektraIsReferenceRedundant (const char * reference);
char * elektraResolveReference (const char * reference, const Key * baseKey, const Key * parentKey);

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

kdb_boolean_t calculateSpecificationToken (char hash_string[65], KeySet * ks, Key * parentKey);

#ifdef __cplusplus
}
}
#endif


#endif

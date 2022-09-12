#ifndef KDBEASE_H
#define KDBEASE_H

#include <kdb.h>
#include <kdbtypes.h>

#ifdef __cplusplus
namespace ckdb
{
extern "C" {
#endif

int elektraKsFilter (ElektraKeyset * result, ElektraKeyset * input, int (*filter) (const ElektraKey * k, void * argument), void * argument);
int elektraKsToMemArray (ElektraKeyset * ks, ElektraKey ** buffer);

int elektraArrayIncName (ElektraKey * key);
int elektraArrayDecName (ElektraKey * key);

int elektraArrayValidateName (const ElektraKey * key);
int elektraArrayValidateBaseNameString (const char * baseName);

const char * elektraKeyGetRelativeName (ElektraKey const * cur, ElektraKey const * parentKey);

ElektraKeyset * elektraArrayGet (const ElektraKey * arrayParent, ElektraKeyset * keys);
ElektraKey * elektraArrayGetNextKey (ElektraKeyset * arrayKeys);

elektraKeyFlags keyCompare (const ElektraKey * key1, const ElektraKey * key2);
elektraKeyFlags keyCompareMeta (const ElektraKey * key1, const ElektraKey * key2);

int elektraIsReferenceRedundant (const char * reference);
char * elektraResolveReference (const char * reference, const ElektraKey * baseKey, const ElektraKey * parentKey);

int elektraKeyToString (const ElektraKey * key, const char ** variable);
int elektraKeyToBoolean (const ElektraKey * key, kdb_boolean_t * variable);
int elektraKeyToChar (const ElektraKey * key, kdb_char_t * variable);
int elektraKeyToOctet (const ElektraKey * key, kdb_octet_t * variable);
int elektraKeyToShort (const ElektraKey * key, kdb_short_t * variable);
int elektraKeyToUnsignedShort (const ElektraKey * key, kdb_unsigned_short_t * variable);
int elektraKeyToLong (const ElektraKey * key, kdb_long_t * variable);
int elektraKeyToUnsignedLong (const ElektraKey * key, kdb_unsigned_long_t * variable);
int elektraKeyToLongLong (const ElektraKey * key, kdb_long_long_t * variable);
int elektraKeyToUnsignedLongLong (const ElektraKey * key, kdb_unsigned_long_long_t * variable);
int elektraKeyToFloat (const ElektraKey * key, kdb_float_t * variable);
int elektraKeyToDouble (const ElektraKey * key, kdb_double_t * variable);

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

int elektraKeyToLongDouble (const ElektraKey * key, kdb_long_double_t * variable);

char * elektraLongDoubleToString (kdb_long_double_t value);

#endif // ELEKTRA_HAVE_KDB_LONG_DOUBLE

kdb_boolean_t calculateSpecificationToken (char hash_string[65], ElektraKeyset * ks, ElektraKey * parentKey);

#ifdef __cplusplus
}
}
#endif


#endif

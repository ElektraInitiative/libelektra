/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_H
#define ELEKTRA_H

#include "kdbtypes.h"

typedef struct _ElektraError ElektraError;
typedef struct _Elektra Elektra;

Elektra * elektraOpen (const char * application);
void elektraClose (Elektra * elektra);

kdb_boolean_t elektraHasError (const Elektra * elektra);
const char * elektraErrorMessage (const Elektra * elektra);
void elektraClearError (Elektra * elektra);

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

#endif //ELEKTRA_H

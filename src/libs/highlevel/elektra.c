/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra.h"
#include <stdlib.h>
#include <memory.h>
#include "kdbprivate.h"
#include "elektra_private.h"
#include "elektra_error_private.h"

#include "stdio.h"

#define READ_KEY \
Key * const nameKey = keyDup (elektra->parentKey); \
keyAddName (nameKey, name); \
\
Key * const resultKey = ksLookup (elektra->config, nameKey, 0); \
if (resultKey == NULL) \
{ \
    exit (EXIT_FAILURE); \
} \
const char * string = keyString (resultKey); \
\
keyDel (nameKey); \

#define CHECK_TYPE(type) \
if (strcmp (keyString (keyGetMeta (resultKey, "type")), type)) \
{ \
    exit (EXIT_FAILURE); \
} \

#define RETURN_VALUE \
return value; \

Elektra * elektraOpen (const char * application, ElektraError ** error)
{
    Key * const parentKey = keyNew (application, KEY_END);
    KDB * const kdb = kdbOpen (parentKey);

    if (kdb == NULL)
    {
        *error = elektraErrorCreateFromKey(parentKey);
        return NULL;
    }

    KeySet * const config = ksNew (0, KS_END);
    const int kdbGetResult = kdbGet (kdb, config, parentKey);

    if (kdbGetResult == -1)
    {
        *error = elektraErrorCreateFromKey(parentKey);
        return NULL;
    }

    Elektra * const elektra = elektraCalloc (sizeof (struct _Elektra));
    elektra->kdb = kdb;
    elektra->parentKey = parentKey;
    elektra->config = config;

    return elektra;
}

void elektraClose (Elektra * elektra)
{
    kdbClose (elektra->kdb, elektra->parentKey);
    ksDel (elektra->config);
    keyDel (elektra->parentKey);

    elektraFree (elektra);
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
const char * elektraGetString (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("string")

    const char * value = string;

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("boolean")

    kdb_boolean_t value = KDB_STRING_TO_BOOLEAN(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_char_t elektraGetChar (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("char")

    const char value = KDB_STRING_TO_CHAR(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_octet_t elektraGetOctet (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("octet")

    const kdb_octet_t value = KDB_STRING_TO_OCTET(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_short_t elektraGetShort (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("short")

    const kdb_short_t value = KDB_STRING_TO_SHORT(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("unsigned_short");

    const kdb_unsigned_short_t value = KDB_STRING_TO_UNSIGNED_SHORT(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_long_t elektraGetLong (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("long")

    const kdb_long_t value = KDB_STRING_TO_LONG(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("unsigned_long")

    const kdb_unsigned_long_t value = KDB_STRING_TO_UNSIGNED_LONG(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("long_long")

    const kdb_long_long_t value = KDB_STRING_TO_LONG_LONG(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("unsigned_long_long")

    const kdb_unsigned_long_long_t value = KDB_STRING_TO_UNSIGNED_LONG_LONG(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_float_t elektraGetFloat (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("float")

    const kdb_float_t value = KDB_STRING_TO_FLOAT(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_double_t elektraGetDouble (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("double")

    const kdb_double_t value = KDB_STRING_TO_DOUBLE(string);

    RETURN_VALUE
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * name)
{
    READ_KEY
    CHECK_TYPE("long_double")

    const kdb_long_double_t value = KDB_STRING_TO_LONG_DOUBLE(string);

    RETURN_VALUE
}

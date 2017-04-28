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

static Key * generateLookupKey (Elektra * elektra, const char * name);
static const char * getValueAsString (Elektra * elektra, const char * name, const char * type);
static const char * getArrayElementValueAsString (Elektra * elektra, const char * name, const char * type, size_t index);
static Key * lookup(Elektra * elektra, Key * key);
static void checkType (Key * key, const char * type);

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
    elektra->lookupKey = keyNew(NULL);

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
    return getValueAsString(elektra, name, "string");
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_BOOLEAN(getValueAsString(elektra, name, "boolean"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_char_t elektraGetChar (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_CHAR(getValueAsString(elektra, name, "char"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_octet_t elektraGetOctet (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_OCTET(getValueAsString(elektra, name, "octet"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_short_t elektraGetShort (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_SHORT(getValueAsString(elektra, name, "short"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_UNSIGNED_SHORT(getValueAsString(elektra, name, "unsigned_short"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_long_t elektraGetLong (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_LONG(getValueAsString(elektra, name, "long"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_UNSIGNED_LONG(getValueAsString(elektra, name, "unsigned_long"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_LONG_LONG(getValueAsString(elektra, name, "long_long"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_UNSIGNED_LONG_LONG(getValueAsString(elektra, name, "unsigned_long_long"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_float_t elektraGetFloat (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_FLOAT(getValueAsString(elektra, name, "float"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_double_t elektraGetDouble (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_DOUBLE(getValueAsString(elektra, name, "double"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 */
kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_LONG_DOUBLE(getValueAsString(elektra, name, "long_double"));
}

/**
 * @param elektra The elektra instance initialized with the parent key.
 * @param name The keyname to look up. The keyname is appended to the parent key.
 * @param index The array index of the desired element, starting with 0.
 */
const char * elektraGetStringArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return getArrayElementValueAsString(elektra, name, "string", index);
}

// Private functions

static const char * getValueAsString (Elektra * elektra, const char * name, const char * type)
{
    Key * const key = generateLookupKey (elektra, name);

    Key * const resultKey = ksLookup (elektra->config, key, 0);
    if (resultKey == NULL)
    {
        printf ("Key not found: %s\n", keyName(key));
        exit (EXIT_FAILURE);
    }

    checkType(resultKey, type);

    return keyString (resultKey);
}

static const char * getArrayElementValueAsString (Elektra * elektra, const char * name, const char * type, size_t index)
{
    Key * const key = generateLookupKey(elektra, name);

    char arrayPart[ELEKTRA_MAX_ARRAY_SIZE];
    elektraWriteArrayNumber (arrayPart, index);
    keyAddName (key, arrayPart);

    Key * const resultKey = lookup(elektra, key);

    checkType(resultKey, type);

    return keyString (resultKey);
}

static Key * generateLookupKey (Elektra * elektra, const char * name)
{
    Key * const lookupKey = elektra->lookupKey;

    keySetName (lookupKey, keyName (elektra->parentKey));
    keyAddName (lookupKey, name);

    return lookupKey;
}

static Key * lookup(Elektra * elektra, Key * key)
{
    Key * const resultKey = ksLookup (elektra->config, key, 0);
    if (resultKey == NULL)
    {
        printf ("Key not found: %s\n", keyName(key));
        exit (EXIT_FAILURE);
    }

    return resultKey;
}

static void checkType (Key * key, const char * type)
{
    if (strcmp (keyString (keyGetMeta (key, "type")), type))
    {
        printf ("Wrong type. Should be: %s\n", type);
        exit (EXIT_FAILURE);
    }
}

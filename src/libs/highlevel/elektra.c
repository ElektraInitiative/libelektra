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

typedef const char * KDBType;
static KDBType KDB_TYPE_STRING = "string";
static KDBType KDB_TYPE_BOOLEAN = "boolean";
static KDBType KDB_TYPE_CHAR = "char";
static KDBType KDB_TYPE_OCTET = "octet";
static KDBType KDB_TYPE_SHORT = "short";
static KDBType KDB_TYPE_UNSIGNED_SHORT = "unsigned_short";
static KDBType KDB_TYPE_LONG = "long";
static KDBType KDB_TYPE_UNSIGNED_LONG = "unsigned_long";
static KDBType KDB_TYPE_LONG_LONG = "long_long";
static KDBType KDB_TYPE_UNSIGNED_LONG_LONG = "unsigned_long_long";
static KDBType KDB_TYPE_FLOAT = "float";
static KDBType KDB_TYPE_LONG_DOUBLE = "long_double";
static KDBType KDB_TYPE_DOUBLE = "double";

static Key * generateLookupKey (Elektra * elektra, const char * name);
static Key * lookup(Elektra * elektra, Key * key);
static void checkType (Key * key, KDBType type);

void setValueAsString (Elektra * elektra, const char * name, const char * value, KDBType type);
static const char * getValueAsString (Elektra * elektra, const char * name, KDBType type);

void setArrayElementValueAsString (Elektra * elektra, const char * name, const char * value, KDBType type, size_t index);
static const char * getArrayElementValueAsString (Elektra * elektra, const char * name, KDBType type, size_t index);


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
    keyDel (elektra->parentKey);
    ksDel (elektra->config);
    keyDel (elektra->lookupKey);

    elektraFree (elektra);
}

// Primitive setters

void elektraSetString (Elektra * elektra, const char * name, const char * value)
{
    setValueAsString(elektra, name, value, KDB_TYPE_STRING);
}

void elektraSetBoolean (Elektra * elektra, const char * name, kdb_boolean_t value)
{
    setValueAsString(elektra, name, KDB_BOOLEAN_TO_STRING(value), KDB_TYPE_BOOLEAN);
}

void elektraSetChar (Elektra * elektra, const char * name, kdb_char_t value)
{
    setValueAsString(elektra, name, KDB_CHAR_TO_STRING(value), KDB_TYPE_CHAR);
}

void elektraSetOctet (Elektra * elektra, const char * name, kdb_octet_t value)
{
    setValueAsString(elektra, name, KDB_OCTET_TO_STRING(value), KDB_TYPE_OCTET);
}

void elektraSetShort (Elektra * elektra, const char * name, kdb_short_t value)
{
    setValueAsString(elektra, name, KDB_SHORT_TO_STRING(value), KDB_TYPE_SHORT);
}

void elektraSetUnsignedShort (Elektra * elektra, const char * name, kdb_unsigned_short_t value)
{
    setValueAsString(elektra, name, KDB_UNSIGNED_SHORT_TO_STRING(value), KDB_TYPE_UNSIGNED_SHORT);
}

void elektraSetLong (Elektra * elektra, const char * name, kdb_long_t value)
{
    setValueAsString(elektra, name, KDB_LONG_TO_STRING(value), KDB_TYPE_LONG);
}

void elektraSetUnsignedLong (Elektra * elektra, const char * name, kdb_unsigned_long_t value)
{
    setValueAsString(elektra, name, KDB_UNSIGNED_LONG_TO_STRING(value), KDB_TYPE_UNSIGNED_LONG);
}

void elektraSetLongLong (Elektra * elektra, const char * name, kdb_long_long_t value)
{
    setValueAsString(elektra, name, KDB_LONG_LONG_TO_STRING(value), KDB_TYPE_LONG_LONG);
}

void elektraSetUnsignedLongLong (Elektra * elektra, const char * name, kdb_unsigned_long_long_t value)
{
    setValueAsString(elektra, name, KDB_UNSIGNED_LONG_LONG_TO_STRING(value), KDB_TYPE_UNSIGNED_LONG_LONG);
}

void elektraSetFloat (Elektra * elektra, const char * name, kdb_float_t value)
{
    setValueAsString(elektra, name, KDB_FLOAT_TO_STRING(value), KDB_TYPE_FLOAT);
}

void elektraSetDouble (Elektra * elektra, const char * name, kdb_double_t value)
{
    setValueAsString(elektra, name, KDB_DOUBLE_TO_STRING(value), KDB_TYPE_DOUBLE);
}

void elektraSetLongDouble (Elektra * elektra, const char * name, kdb_long_double_t value)
{
    setValueAsString(elektra, name, KDB_LONG_DOUBLE_TO_STRING(value), KDB_TYPE_LONG_DOUBLE);
}

// Array setters

void elektraSetStringArrayElement (Elektra * elektra, const char * name, const char * value, size_t index)
{
    setArrayElementValueAsString(elektra, name, value, KDB_TYPE_STRING, index);
}

void elektraSetBooleanArrayElement (Elektra * elektra, const char * name, kdb_boolean_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_BOOLEAN_TO_STRING(value), KDB_TYPE_BOOLEAN, index);
}

void elektraSetCharArrayElement (Elektra * elektra, const char * name, kdb_char_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_CHAR_TO_STRING(value), KDB_TYPE_CHAR, index);
}

void elektraSetOctetArrayElement (Elektra * elektra, const char * name, kdb_octet_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_OCTET_TO_STRING(value), KDB_TYPE_OCTET, index);
}

void elektraSetShortArrayElement (Elektra * elektra, const char * name, kdb_short_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_SHORT_TO_STRING(value), KDB_TYPE_SHORT, index);
}

void elektraSetUnsignedShortArrayElement (Elektra * elektra, const char * name, kdb_unsigned_short_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_UNSIGNED_SHORT_TO_STRING(value), KDB_TYPE_UNSIGNED_SHORT, index);
}

void elektraSetLongArrayElement (Elektra * elektra, const char * name, kdb_long_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_LONG_TO_STRING(value), KDB_TYPE_LONG, index);
}

void elektraSetUnsignedLongArrayElement (Elektra * elektra, const char * name, kdb_unsigned_long_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_UNSIGNED_LONG_TO_STRING(value), KDB_TYPE_UNSIGNED_LONG, index);
}

void elektraSetLongLongArrayElement (Elektra * elektra, const char * name, kdb_long_long_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_LONG_LONG_TO_STRING(value), KDB_TYPE_LONG_LONG, index);
}

void elektraSetUnsignedLongLongArrayElement (Elektra * elektra, const char * name, kdb_unsigned_long_long_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_UNSIGNED_LONG_TO_STRING(value), KDB_TYPE_UNSIGNED_LONG_LONG, index);
}

void elektraSetFloatArrayElement (Elektra * elektra, const char * name, kdb_float_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_FLOAT_TO_STRING(value), KDB_TYPE_FLOAT, index);
}

void elektraSetDoubleArrayElement (Elektra * elektra, const char * name, kdb_double_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_DOUBLE_TO_STRING(value), KDB_TYPE_DOUBLE, index);
}

void elektraSetLongDoubleArrayElement (Elektra * elektra, const char * name, kdb_long_double_t value, size_t index)
{
    setArrayElementValueAsString(elektra, name, KDB_LONG_DOUBLE_TO_STRING(value), KDB_TYPE_LONG_DOUBLE, index);
}

// Primitive getters

const char * elektraGetString (Elektra * elektra, const char * name)
{
    return getValueAsString (elektra, name, KDB_TYPE_STRING);
}

kdb_boolean_t elektraGetBoolean (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_BOOLEAN (getValueAsString (elektra, name, KDB_TYPE_BOOLEAN));
}

kdb_char_t elektraGetChar (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_CHAR (getValueAsString (elektra, name, KDB_TYPE_CHAR));
}

kdb_octet_t elektraGetOctet (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_OCTET (getValueAsString (elektra, name, KDB_TYPE_OCTET));
}

kdb_short_t elektraGetShort (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_SHORT (getValueAsString (elektra, name, KDB_TYPE_SHORT));
}

kdb_unsigned_short_t elektraGetUnsignedShort (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_UNSIGNED_SHORT (getValueAsString (elektra, name, KDB_TYPE_UNSIGNED_SHORT));
}

kdb_long_t elektraGetLong (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_LONG (getValueAsString (elektra, name, KDB_TYPE_LONG));
}

kdb_unsigned_long_t elektraGetUnsignedLong (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_UNSIGNED_LONG (getValueAsString (elektra, name, KDB_TYPE_UNSIGNED_LONG));
}

kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_LONG_LONG (getValueAsString (elektra, name, KDB_TYPE_LONG_LONG));
}

kdb_unsigned_long_long_t elektraGetUnsignedLongLong (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_UNSIGNED_LONG_LONG (getValueAsString (elektra, name, KDB_TYPE_UNSIGNED_LONG_LONG));
}

kdb_float_t elektraGetFloat (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_FLOAT (getValueAsString (elektra, name, KDB_TYPE_FLOAT));
}

kdb_double_t elektraGetDouble (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_DOUBLE (getValueAsString (elektra, name, KDB_TYPE_DOUBLE));
}

kdb_long_double_t elektraGetLongDouble (Elektra * elektra, const char * name)
{
    return KDB_STRING_TO_LONG_DOUBLE (getValueAsString (elektra, name, KDB_TYPE_LONG_DOUBLE));
}

// Array getters

size_t elektraArraySize (Elektra * elektra, const char * name)
{
    Key * const key = generateLookupKey (elektra, name);

    KeySet * arrayKeys = elektraArrayGet (key, elektra->config);
    size_t size = (size_t) ksGetSize(arrayKeys);
    ksDel(arrayKeys);

    return size;
}

const char * elektraGetStringArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return getArrayElementValueAsString (elektra, name, KDB_TYPE_STRING, index);
}

kdb_boolean_t elektraGetBooleanArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_BOOLEAN (getArrayElementValueAsString (elektra, name, KDB_TYPE_BOOLEAN, index));
}

kdb_char_t elektraGetCharArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_CHAR (getArrayElementValueAsString (elektra, name, KDB_TYPE_CHAR, index));
}

kdb_octet_t elektraGetOctetArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_OCTET (getArrayElementValueAsString (elektra, name, KDB_TYPE_OCTET, index));
}

kdb_short_t elektraGetShortArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_SHORT (getArrayElementValueAsString (elektra, name, KDB_TYPE_SHORT, index));
}

kdb_unsigned_short_t elektraGetUnsignedShortArrayElement  (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_UNSIGNED_SHORT (getArrayElementValueAsString (elektra, name, KDB_TYPE_UNSIGNED_SHORT, index));
}

kdb_long_t elektraGetLongArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_LONG (getArrayElementValueAsString (elektra, name, KDB_TYPE_LONG, index));
}

kdb_unsigned_long_t elektraGetUnsignedLongArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_UNSIGNED_LONG (getArrayElementValueAsString (elektra, name, KDB_TYPE_UNSIGNED_LONG, index));
}

kdb_long_long_t elektraGetLongLongArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_LONG_LONG (getArrayElementValueAsString (elektra, name, KDB_TYPE_LONG_LONG, index));
}

kdb_unsigned_long_long_t elektraGetUnsignedLongLongArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_UNSIGNED_LONG_LONG (getArrayElementValueAsString (elektra, name, KDB_TYPE_UNSIGNED_LONG_LONG, index));
}

kdb_float_t elektraGetFloatArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_FLOAT (getArrayElementValueAsString (elektra, name, KDB_TYPE_FLOAT, index));
}

kdb_double_t elektraGetDoubleArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_DOUBLE (getArrayElementValueAsString (elektra, name, KDB_TYPE_DOUBLE, index));
}

kdb_long_double_t elektraGetLongDoubleArrayElement (Elektra * elektra, const char * name, size_t index)
{
    return KDB_STRING_TO_LONG_DOUBLE (getArrayElementValueAsString (elektra, name, KDB_TYPE_LONG_DOUBLE, index));
}

// Private functions

void setValueAsString (Elektra * elektra, const char * name, const char * value, KDBType type)
{
    int ret = 0;
    do
    {
        Key * const lookupKey = generateLookupKey (elektra, name);
        Key * const key = lookup (elektra, lookupKey);
        checkType (key, type);

        keySetString (key, value);

        ret = kdbSet (elektra->kdb, elektra->config, elektra->parentKey);
        if (ret == -1)
        {
            Key * problemKey = ksCurrent (elektra->config);
            if (problemKey != NULL)
            {
                printf ("problemKey: %s\n", keyName (problemKey));
            }

            kdbGet (elektra->kdb, elektra->config, elektra->parentKey);
        }
    } while (ret == -1);
}

static const char * getValueAsString (Elektra * elektra, const char * name, KDBType type)
{
    Key * const lookupKey = generateLookupKey (elektra, name);
    Key * const resultKey = lookup (elektra, lookupKey);
    checkType (resultKey, type);

    return keyString (resultKey);
}

void setArrayElementValueAsString (Elektra * elektra, const char * name, const char * value, KDBType type, size_t index)
{
    int ret = 0;
    do
    {
        Key * const lookupKey = generateLookupKey (elektra, name);

        char arrayPart[ELEKTRA_MAX_ARRAY_SIZE];
        elektraWriteArrayNumber (arrayPart, index);
        keyAddName (lookupKey, arrayPart);

        Key * const key = lookup (elektra, lookupKey);

        checkType (key, type);

        keySetString (key, value);

        ret = kdbSet (elektra->kdb, elektra->config, elektra->parentKey);
        if (ret == -1)
        {
            Key * problemKey = ksCurrent (elektra->config);
            if (problemKey != NULL)
            {
                printf ("problemKey: %s\n", keyName (problemKey));
            }

            kdbGet (elektra->kdb, elektra->config, elektra->parentKey);
        }
    } while (ret == -1);
}

static const char * getArrayElementValueAsString (Elektra * elektra, const char * name, KDBType type, size_t index)
{
    Key * const lookupKey = generateLookupKey (elektra, name);

    char arrayPart[ELEKTRA_MAX_ARRAY_SIZE];
    elektraWriteArrayNumber (arrayPart, index);
    keyAddName (lookupKey, arrayPart);

    Key * const resultKey = lookup (elektra, lookupKey);

    checkType (resultKey, type);

    return keyString (resultKey);
}

static Key * generateLookupKey (Elektra * elektra, const char * name)
{
    Key * const lookupKey = elektra->lookupKey;

    keySetName (lookupKey, keyName (elektra->parentKey));
    keyAddName (lookupKey, name);

    return lookupKey;
}

static Key * lookup (Elektra * elektra, Key * key)
{
    Key * const resultKey = ksLookup (elektra->config, key, 0);
    if (resultKey == NULL)
    {
        printf ("Key not found: %s\n", keyName (key));
        exit (EXIT_FAILURE);
    }

    return resultKey;
}

static void checkType (Key * key, KDBType type)
{
    if (strcmp (keyString (keyGetMeta (key, "type")), type))
    {
        printf ("Wrong type. Should be: %s\n", type);
        exit (EXIT_FAILURE);
    }
}

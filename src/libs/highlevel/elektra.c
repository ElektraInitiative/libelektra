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

Elektra * elektraOpen (const char * application)
{
    Key * const parentKey = keyNew (application, KEY_END);
    KDB * const kdb = kdbOpen (parentKey);

    KeySet * const config = ksNew (0, KS_END);
    kdbGet (kdb, config, parentKey);

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

    elektraClearError (elektra);
    elektraFree (elektra);
}

kdb_boolean_t elektraHasError (const Elektra * elektra)
{
    return elektra->error != NULL;
}

const char * elektraErrorMessage (const Elektra * elektra)
{
    return elektra->error->msg;
}

void elektraClearError (Elektra * elektra)
{
    elektraFree (elektra->error);
    elektra->error = NULL;
}

const char * elektraGetString (Elektra * elektra, const char * name)
{
    Key * const nameKey = keyDup (elektra->parentKey);
    keyAddName (nameKey, name);

    Key * const resultKey = ksLookup (elektra->config, nameKey, 0);
    const char * string = keyString (resultKey);

    keyDel (nameKey);

    char * value = elektraMalloc (keyGetValueSize(resultKey));
    strcpy (value, string);

    keyDel (resultKey);

    return value;
}

kdb_long_long_t elektraGetLongLong (Elektra * elektra, const char * name)
{
    Key * const nameKey = keyDup (elektra->parentKey);
    keyAddName (nameKey, name);

    Key * const resultKey = ksLookup (elektra->config, nameKey, 0);
    const char * string = keyString (resultKey);

    keyDel (nameKey);

    const kdb_long_long_t value = ELEKTRA_LONG_LONG_S (string, NULL, 10);

    keyDel (resultKey);

    return value;
}

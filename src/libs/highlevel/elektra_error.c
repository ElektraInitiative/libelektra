/**
 * @file
 *
 * @brief Elektra error codes.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <string.h>
#include "elektra_error.h"
#include "elektra_error_private.h"
#include "kdbhelper.h"
#include "kdbprivate.h"

// elektra_error_private.h

ElektraError * elektraErrorCreate (ElektraErrorCode code, const char * description, ElektraErrorSeverity severity, ElektraErrorGroup group, ElektraErrorModule module)
{
    ElektraError * const error = elektraCalloc (sizeof (struct _ElektraError));
    error->code = code;
    error->description = description;
    error->severity = severity;
    error->group = group;
    error->module = module;

    return error;
}

#define ELEKTRA_ERROR_SEVERITY

ElektraError * elektraErrorCreateFromKey (Key * key)
{
    const Key * metaKey = keyGetMeta (key, "error");

    if (NULL == metaKey) {
        return NULL;
    }

    ElektraErrorCode code = KDB_STRING_TO_SHORT(keyString (keyGetMeta (key, "error/number")));
    const char * description = keyString (keyGetMeta (key, "error/description"));

    const char * severityString = keyString (keyGetMeta (key, "error/severity"));
    ElektraErrorSeverity severity = ELEKTRA_ERROR_SEVERITY_FATAL; // Default is FATAL.
    if (!elektraStrCmp(severityString, "error")) {
        severity = ELEKTRA_ERROR_SEVERITY_ERROR;
    } else if (!elektraStrCmp(severityString, "warning")) {
        severity = ELEKTRA_ERROR_SEVERITY_WARNING;
    }

    ElektraErrorGroup group = keyString (keyGetMeta (key, "error/ingroup"));
    ElektraErrorModule module = keyString (keyGetMeta (key, "error/module"));

    return elektraErrorCreate(code, description, severity, group, module);
}

// elektra_error.h

ElektraErrorCode elektraErrorCode (ElektraError * error)
{
    return error->code;
}

const char * elektraErrorDescription (ElektraError * error)
{
    return error->description;
}

ElektraErrorSeverity elektraErrorSeverity (ElektraError * error)
{
    return error->severity;
}

ElektraErrorGroup elektraErrorGroup (ElektraError * error)
{
    return error->group;
}

ElektraErrorModule elektraErrorModule (ElektraError * error)
{
    return error->module;
}

void elektraErrorFree (ElektraError * error)
{
    elektraFree(error);
}

// Private

static void removeMetaData (Key * key, const char * searchfor)
{
    const Key * iter_key;
    keyRewindMeta (key);
    while ((iter_key = keyNextMeta (key)) != 0)
    {
        // startsWith
        if (strncmp (searchfor, keyName (iter_key), strlen (searchfor)) == 0)
        {
            keySetMeta (key, keyName (iter_key), 0);
        }
    }
}

/**
 * @file
 *
 * @brief Elektra error codes.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra_error.h"
#include "elektra_error_private.h"
#include "kdbhelper.h"

ElektraError * elektraErrorCreate (int code, const char * message)
{
    ElektraError * const error = elektraCalloc (sizeof (struct _ElektraError));
    error->code = code;
    error->message = message;

    return error;
}

ElektraErrorCode elektraErrorCode(ElektraError * error)
{
    return error->code;
}

const char * elektraErrorMessage(ElektraError * error)
{
    return error->message;
}

void elektraErrorFree (ElektraError * error)
{
    elektraFree(error);
}


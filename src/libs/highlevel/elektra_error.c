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
#include "kdbprivate.h"
#include <string.h>

// elektra_error_private.h

ElektraError * elektraErrorCreate (ElektraErrorCode code, const char * description, ElektraErrorSeverity severity, ElektraErrorGroup group,
				   ElektraErrorModule module)
{
	ElektraError * const error = elektraCalloc (sizeof (struct _ElektraError));
	error->code = code;
	error->description = description;
	error->severity = severity;
	error->group = group;
	error->module = module;

	return error;
}

ElektraError * elektraErrorCreateFromKey (Key * key)
{
	const Key * metaKey = keyGetMeta (key, "error");

	if (NULL == metaKey)
	{
		return NULL;
	}

	ElektraErrorCode code = KDB_STRING_TO_LONG (keyString (keyGetMeta (key, "error/number")));
	const char * description = keyString (keyGetMeta (key, "error/description"));

	const char * severityString = keyString (keyGetMeta (key, "error/severity"));
	ElektraErrorSeverity severity = ELEKTRA_ERROR_SEVERITY_FATAL; // Default is FATAL.
	if (!elektraStrCmp (severityString, "error"))
	{
		severity = ELEKTRA_ERROR_SEVERITY_ERROR;
	}
	else if (!elektraStrCmp (severityString, "warning"))
	{
		severity = ELEKTRA_ERROR_SEVERITY_WARNING;
	}

	ElektraErrorGroup group = keyString (keyGetMeta (key, "error/ingroup"));
	ElektraErrorModule module = keyString (keyGetMeta (key, "error/module"));

	return elektraErrorCreate (code, description, severity, group, module);
}

// elektra_error.h

/**
 * \addtogroup highlevel High-level API
 * @{
 */

/**
 * @return Code of the given error.
 */
ElektraErrorCode elektraErrorCode (ElektraError * error)
{
	return error->code;
}

/**
 * @return Description for the given error.
 */
const char * elektraErrorDescription (ElektraError * error)
{
	return error->description;
}

/**
 * @return Severity of the given error.
 */
ElektraErrorSeverity elektraErrorSeverity (ElektraError * error)
{
	return error->severity;
}

/**
 * @return Group of the given error.
 */
ElektraErrorGroup elektraErrorGroup (ElektraError * error)
{
	return error->group;
}

/**
 * @return Module of the given error.
 */
ElektraErrorModule elektraErrorModule (ElektraError * error)
{
	return error->module;
}

/**
 * @brief Frees memory used by the error and sets the referenced error variable to NULL.
 */
void elektraErrorReset (ElektraError ** error)
{
	if (*error == NULL)
	{
		return;
	}

	elektraFree (*error);
	*error = NULL;
}

/**
 * @}
 */

/**
 * @file
 *
 * @brief Elektra error codes.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra_error.h"
#include "elektra_conversion.h"
#include "elektra_error_private.h"
#include "elektra_errors_private.h"
#include "kdberrors.h"
#include "kdbhelper.h"
#include <string.h>

static ElektraKDBError * elektraKDBErrorFromKey (Key * key);

// elektra_error_private.h

/**
 * Creates a new ElektraError using the provided values.
 * The returned value will be allocated with elektraCalloc().
 *
 * @param code        The error code of the error.
 * @param description The description of the error.
 * @param severity    The severity of the error. Only use ELEKTRA_ERROR_SEVERITY_FATAL,
 *                    if the error will be raised with elektraFatalError().
 * @param group       The group to which this error belongs.
 * @param module      The module from which this error originates.
 * @return A newly allocated ElektraError (free with elektraFree()).
 */
ElektraError * elektraErrorCreate (ElektraErrorCode code, const char * description, ElektraErrorSeverity severity)
{
	ElektraError * const error = elektraCalloc (sizeof (struct _ElektraError));
	error->code = code;
	error->description = elektraStrDup (description);
	error->severity = severity;

	return error;
}

/**
 * Creates a new ElektraError by using the values of the error metadata of a Key.
 *
 * @param Key The key from which the error data shall be taken.
 * @return A new ElektraError created with elektraErrorCreate().
 */
ElektraError * elektraErrorCreateFromKey (Key * key)
{
	const Key * metaKey = keyGetMeta (key, "error");

	if (NULL == metaKey)
	{
		return NULL;
	}

	kdb_long_t code;
	elektraKeyToLong (keyGetMeta (key, "error/number"), &code);

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

	ElektraKDBErrorGroup group = keyString (keyGetMeta (key, "error/ingroup"));
	ElektraKDBErrorModule module = keyString (keyGetMeta (key, "error/module"));


	ElektraError * error;

	const char * description = keyString (keyGetMeta (key, "error/description"));
	error = elektraErrorLowLevel (severity, code, description, group, module);

	error->lowLevelError = elektraKDBErrorFromKey (key);
	return error;
}

// elektra_error.h

/**
 * \addtogroup highlevel High-level API
 * @{
 */

/**
 * @return the error code of the given error
 */
ElektraErrorCode elektraErrorCode (const ElektraError * error)
{
	return error->code;
}

/**
 * @return the description for the given error
 */
const char * elektraErrorDescription (const ElektraError * error)
{
	return error->description;
}

/**
 * @return the severity of the given error
 */
ElektraErrorSeverity elektraErrorSeverity (const ElektraError * error)
{
	return error->severity;
}

/**
 * @return the Key from which this error was extracted,
 * or NULL if the error was created by the high-level API
 */
ElektraKDBError * elektraErrorLowLevelError (const ElektraError * error)
{
	return error->lowLevelError;
}

/**
 * @return the error code of the given kdb error
 */
int elektraKDBErrorCode (const ElektraKDBError * error)
{
	return error->code;
}

/**
 * @return the description for the given kdb error
 */
const char * elektraKDBErrorDescription (const ElektraKDBError * error)
{
	return error->description;
}

/**
 * @return the severity of the given kdb error
 */
ElektraErrorSeverity elektraKDBErrorSeverity (const ElektraKDBError * error)
{
	return error->severity;
}

/**
 * @return the group from which the given kdb error originated
 */
ElektraKDBErrorGroup elektraKDBErrorGroup (const ElektraKDBError * error)
{
	return error->group;
}

/**
 * @return the module from which the given kdb error originated
 */
ElektraKDBErrorModule elektraKDBErrorModule (const ElektraKDBError * error)
{
	return error->module;
}

/**
 * @return the reason for the given kdb error
 */
const char * elektraKDBErrorReason (const ElektraKDBError * error)
{
	return error->reason;
}

/**
 * @return the number of warnings associated with the given kdb error
 */
int elektraKDBErrorWarningCount (const ElektraKDBError * error)
{
	return error->warningCount;
}

/**
 * @return the array of warnings associated with the given kdb error
 */
const ElektraKDBError ** elektraKDBErrorWarnings (const ElektraKDBError * error)
{
	return (const ElektraKDBError **) error->warnings;
}

/**
 * @return the Key from which the given kdb error was extracted
 */
Key * elektraKDBErrorKey (const ElektraKDBError * error)
{
	return error->errorKey;
}

/**
 * Frees the memory used by the error and sets
 * the referenced error variable to NULL.
 */
void elektraErrorReset (ElektraError ** error)
{
	if (*error == NULL)
	{
		return;
	}

	ElektraError * actualError = *error;

	if (actualError->description != NULL)
	{
		elektraFree (actualError->description);
	}

	if (actualError->lowLevelError != NULL && actualError->lowLevelError->warnings != NULL)
	{
		for (int i = 0; i < actualError->lowLevelError->warningCount; ++i)
		{
			elektraFree (actualError->lowLevelError->warnings[i]);
		}
		elektraFree (actualError->lowLevelError->warnings);
	}

	elektraFree (*error);
	*error = NULL;
}

/**
 * @}
 */

static ElektraKDBError * elektraKDBErrorFromKey (Key * key)
{
	if (key == NULL)
	{
		return NULL;
	}

	kdb_long_t code;
	elektraKeyToLong (keyGetMeta (key, "error/number"), &code);

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

	ElektraKDBErrorGroup group = keyString (keyGetMeta (key, "error/ingroup"));
	ElektraKDBErrorModule module = keyString (keyGetMeta (key, "error/module"));
	const char * reason = keyString (keyGetMeta (key, "error/reason"));
	const char * description = keyString (keyGetMeta (key, "error/description"));

	ElektraKDBError * const error = elektraCalloc (sizeof (struct _ElektraKDBError));
	error->code = code;
	error->description = description;
	error->severity = severity;
	error->group = group;
	error->module = module;
	error->reason = reason;
	error->errorKey = key;

	kdb_long_t warningCount = 0;
	const Key * warningsKey = keyGetMeta (key, "warnings");
	if (warningsKey != NULL)
	{
		elektraKeyToLong (warningsKey, &warningCount);
	}

	error->warningCount = warningCount;
	if (warningCount > 0)
	{
		ElektraKDBError ** warnings = elektraCalloc (warningCount * sizeof (ElektraKDBError *));

		for (int i = 0; i < warningCount; ++i)
		{
			ElektraKDBError * const warning = elektraCalloc (sizeof (struct _ElektraKDBError));
			warning->severity = ELEKTRA_ERROR_SEVERITY_WARNING;

			char * name = elektraFormat ("warnings/#%02d/number", i);
			kdb_long_t warningCode;
			elektraKeyToLong (keyGetMeta (key, name), &warningCode);
			warning->code = warningCode;
			elektraFree (name);

			name = elektraFormat ("warnings/#%02d/description", i);
			warning->description = keyString (keyGetMeta (key, name));
			elektraFree (name);

			name = elektraFormat ("warnings/#%02d/ingroup", i);
			warning->group = keyString (keyGetMeta (key, name));
			elektraFree (name);

			name = elektraFormat ("warnings/#%02d/module", i);
			warning->module = keyString (keyGetMeta (key, name));
			elektraFree (name);

			name = elektraFormat ("warnings/#%02d/reason", i);
			warning->reason = keyString (keyGetMeta (key, name));
			elektraFree (name);

			warning->errorKey = key;
			warnings[i] = warning;
		}
		error->warnings = warnings;
	}
	else
	{
		error->warnings = NULL;
	}

	return error;
}

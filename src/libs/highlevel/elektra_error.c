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
#include "kdbhelper.h"
#include "kdbprivate.h"
#include <elektra_error_private.h>
#include <string.h>


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
ElektraError * elektraErrorCreate (ElektraErrorCode code, const char * description, ElektraErrorSeverity severity, ElektraErrorGroup group,
				   ElektraErrorModule module)
{
	ElektraError * const error = elektraCalloc (sizeof (struct _ElektraError));
	error->code = code;
	error->description = elektraStrDup (description);
	error->severity = severity;
	error->group = group;
	error->module = module;

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

	ElektraErrorCode code;
	elektraKeyToLong (keyGetMeta (key, "error/number"), &code);
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

/**
 * Creates a new Key-Not-Found error.
 * 
 * @param keyname  The name of the key that could not be found.
 * @param moreDesc May be NULL. Additional information about the error placed in the description.
 * @return a new ElektraError created with elektraErrorCreate().
 */
ElektraError * elektraErrorKeyNotFound (const char * keyname, const char * moreDesc)
{
	char * errorString;
	if (moreDesc == NULL)
	{
		errorString = elektraFormat ("Could not find key %s", keyname);
	}
	else
	{
		errorString = elektraFormat ("Could not find key %s\n%s", keyname, moreDesc);
	}
	ElektraError * error = elektraErrorCreate (0 /* TODO */, errorString, ELEKTRA_ERROR_SEVERITY_FATAL, "highlevel", "get");
	elektraFree (errorString);
	return error;
}

/**
 * Creates a new Wrong-Type error.
 * 
 * @param keyname      The name of the Key that had unexpected type metadata
 * @param actualType   The actual type set in the metadata of the Key.
 * @param expectedType The expected type that did not match @p actualType.
 * @param moreDesc     May be NULL. Additional information about the error placed
 *                     in the description.
 * @return a new ElektraError created with elektraErrorCreate().
 */
ElektraError * elektraErrorWrongType (const char * keyname, KDBType actualType, KDBType expectedType, const char * moreDesc)
{
	char * errorString;
	if (moreDesc == NULL)
	{
		errorString = elektraFormat ("Key %s has wrong type; expected: %s but got: %s", keyname, expectedType, actualType);
	}
	else
	{
		errorString =
			elektraFormat ("Key %s has wrong type; expected: %s but got: %s\n%s", keyname, expectedType, actualType, moreDesc);
	}
	ElektraError * error = elektraErrorCreate (0 /* TODO */, errorString, ELEKTRA_ERROR_SEVERITY_FATAL, "highlevel", "get");
	elektraFree (errorString);
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
 * @return the group to which the given error belongs
 */
ElektraErrorGroup elektraErrorGroup (const ElektraError * error)
{
	return error->group;
}

/**
 * @return the module from which the given error originated
 */
ElektraErrorModule elektraErrorModule (const ElektraError * error)
{
	return error->module;
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

	if ((*error)->description != NULL)
	{
		elektraFree ((*error)->description);
	}

	elektraFree (*error);
	*error = NULL;
}

/**
 * @}
 */

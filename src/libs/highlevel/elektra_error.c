/**
 * @file
 *
 * @brief Elektra error codes.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra/error.h"
#include "elektra/conversion.h"
#include "kdbprivate.h"
#include "kdberrors.h"
#include "kdbhelper.h"
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

// elektra/errorprivate.h

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

// elektra/error.h

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

	ElektraKDBError * kdbError = actualError->lowLevelError;
	if (kdbError != NULL)
	{
		if (kdbError->warnings != NULL)
		{
			for (int i = 0; i < kdbError->warningCount; ++i)
			{
				elektraFree (kdbError->warnings[i]);
			}
			elektraFree (kdbError->warnings);
		}
		elektraFree (kdbError);
	}

	elektraFree (actualError);
	*error = NULL;
}

/**
 * @}
 */

#ifdef __cplusplus
};
#endif

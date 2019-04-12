/**
 * @file
 *
 * @brief Elektra error codes.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra/conversion.h"
#include "elektra/error.h"
#include "kdberrors.h"
#include "kdbhelper.h"
#include "kdbprivate.h"
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
 * @return the error code of the attached low-level error,
 * or NULL if no low-level error is attached
 */
const char * elektraKDBErrorCode (const ElektraError * error)
{
	if (error->lowLevelError == NULL)
	{
		return NULL;
	}

	return error->lowLevelError->code;
}

/**
 * @return the description for the attached low-level error,
 * or -1 if no low-level error is attached
 */
const char * elektraKDBErrorDescription (const ElektraError * error)
{
	if (error->lowLevelError == NULL)
	{
		return NULL;
	}

	return error->lowLevelError->description;
}

/**
 * @return the severity of the attached low-level error,
 * or #ELEKTRA_ERROR_SEVERITY_FATAL if no low-level error is attached
 */
ElektraErrorSeverity elektraKDBErrorSeverity (const ElektraError * error)
{
	if (error->lowLevelError == NULL)
	{
		return ELEKTRA_ERROR_SEVERITY_FATAL;
	}

	return error->lowLevelError->severity;
}

/**
 * @return the group from which the attached low-level error originated,
 * or NULL if no low-level error is attached
 */
ElektraKDBErrorGroup elektraKDBErrorGroup (const ElektraError * error)
{
	if (error->lowLevelError == NULL)
	{
		return NULL;
	}

	return error->lowLevelError->group;
}

/**
 * @return the module from which the attached low-level error originated,
 * or NULL if no low-level error is attached
 */
ElektraKDBErrorModule elektraKDBErrorModule (const ElektraError * error)
{
	if (error->lowLevelError == NULL)
	{
		return NULL;
	}

	return error->lowLevelError->module;
}

/**
 * @return the reason for the attached low-level error,
 * or NULL if no low-level error is attached
 */
const char * elektraKDBErrorReason (const ElektraError * error)
{
	if (error->lowLevelError == NULL)
	{
		return NULL;
	}

	return error->lowLevelError->reason;
}

/**
 * @return the number of warnings associated with the attached low-level error,
 * or -1 if no low-level error is attached
 */
int elektraKDBErrorWarningCount (const ElektraError * error)
{
	if (error->lowLevelError == NULL)
	{
		return -1;
	}

	return error->lowLevelError->warningCount;
}

/**
 * @return a newly allocated ElektraError representing the warning at the given index,
 * or NULL if the index is out of range
 *
 * the returned error will always have the following properties:
 * - error code is #ELEKTRA_ERROR_CODE_LOW_LEVEL
 * - description is ""
 * - severity is #ELEKTRA_ERROR_SEVERITY_WARNING
 * - the attached low-level error represents the warning in question
 *
 * NOTE: you have to free the memory allocated by this function using elektraFree()
 */
ElektraError * elektraKDBErrorGetWarning (const ElektraError * error, int index)
{
	if (index < 0 || index > error->lowLevelError->warningCount)
	{
		return NULL;
	}

	ElektraError * warning = elektraErrorCreate (ELEKTRA_ERROR_CODE_LOW_LEVEL, "", ELEKTRA_ERROR_SEVERITY_WARNING);
	warning->lowLevelError = error->lowLevelError->warnings[index];
	return warning;
}

/**
 * @return the Key from which the given kdb error was extracted,
 * or NULL if no low-level error is attached
 */
Key * elektraKDBErrorKey (const ElektraError * error)
{
	if (error->lowLevelError == NULL)
	{
		return NULL;
	}

	return error->lowLevelError->errorKey;
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

	struct _ElektraKDBError * kdbError = actualError->lowLevelError;
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

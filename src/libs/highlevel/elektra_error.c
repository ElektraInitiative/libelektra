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

// kdbprivate.h

/**
 * Creates a new ElektraError using the provided values.
 * The returned value will be allocated with elektraCalloc().
 *
 * @param code        The error code of the error. Must be compile-time constant.
 * @param description The description of the error. Will be copied and stored in the struct.
 * @param module      The module that raised the error. Must be compile-time constant.
 * @param file        The file that raised the error. Must be compile-time constant.
 * @param line        The line in which the error was raised.
 *
 * @return A newly allocated ElektraError (free with elektraErrorReset()).
 */
ElektraError * elektraErrorCreate (const char * code, const char * description, const char * module, const char * file, kdb_long_t line)
{
	ElektraError * const error = elektraCalloc (sizeof (struct _ElektraError));
	error->code = code;
	error->codeFromKey = NULL;
	error->description = elektraStrDup (description);
	error->module = module;
	error->file = file;
	error->line = line;
	error->warningCount = 0;
	error->warningAlloc = 0;
	error->warnings = NULL;
	error->errorKey = NULL;

	return error;
}

/**
 * Adds a warning to an existing ElektraError struct.
 * If you want to report a warning without an error, create a dummy error with
 * elektraErrorPureWarning() and then add a warning to it.
 *
 * @param error   The error to which @p warning shall be added.
 * @param warning The warning to add. Once added it is owned by @p error.
 *                DO NOT call elektraErrorReset() on it afterwards.
 */
void elektraErrorAddWarning (ElektraError * error, ElektraError * warning)
{
	if (error->warningAlloc == 0)
	{
		error->warningCount = 1;
		error->warningAlloc = 4;
		error->warnings = elektraCalloc (error->warningAlloc * sizeof (ElektraError *));
	}
	else
	{
		++error->warningCount;
		if (error->warningCount > error->warningAlloc)
		{
			error->warningAlloc *= 2;
			elektraRealloc ((void **) &error->warnings, error->warningAlloc * sizeof (ElektraError *));
		}
	}

	error->warnings[error->warningCount - 1] = warning;
}

/**
 * Extracts the error and all warnings from the given key.
 * If no error exists, a pure warning error will be used.
 * @see elektraErrorPureWarning
 *
 * @param key The to extract error and warnings from.
 *
 * @return A newly allocated ElektraError (free with elektraErrorReset()).
 */
ElektraError * elektraErrorFromKey (Key * key)
{
	if (key == NULL)
	{
		return NULL;
	}

	ElektraError * error;
	if (keyGetMeta (key, "error") == NULL)
	{
		error = elektraErrorPureWarning ();
	}
	else
	{
		const Key * reasonMeta = keyGetMeta (key, "error/reason");

		const char * codeFromKey = keyString (keyGetMeta (key, "error/number"));
		const char * description = keyString (keyGetMeta (key, "error/description"));
		const char * module = keyString (keyGetMeta (key, "error/module"));
		const char * file = keyString (keyGetMeta (key, "error/file"));

		char * fullDescription =
			reasonMeta != NULL ? elektraFormat ("%s: %s", description, keyString (reasonMeta)) : elektraStrDup (description);

		kdb_long_t line = 0;
		elektraKeyToLong (key, &line);
		error = elektraErrorCreate (NULL, fullDescription, module, file, line);
		error->codeFromKey = elektraStrDup (codeFromKey);
		error->errorKey = key;

		elektraFree (fullDescription);
	}


	kdb_long_t warningCount = 0;
	const Key * warningsKey = keyGetMeta (key, "warnings"); // TODO: read warning count correctly
	if (warningsKey != NULL)
	{
		elektraKeyToLong (warningsKey, &warningCount);
	}

	if (warningCount > 0)
	{
		error->warningAlloc = 4;
		while (error->warningAlloc < warningCount)
		{
			error->warningAlloc *= 2;
		}
		error->warningCount = warningCount;

		error->warnings = elektraCalloc (error->warningAlloc * sizeof (ElektraError *));

		for (int i = 0; i < warningCount; ++i)
		{
			const char * codeFromKey = keyString (keyGetMeta (key, "error/number"));
			const char * description = keyString (keyGetMeta (key, "error/description"));
			const char * module = keyString (keyGetMeta (key, "error/module"));
			const char * file = keyString (keyGetMeta (key, "error/file"));
			kdb_long_t line = 0;
			elektraKeyToLong (key, &line);

			ElektraError * warning = elektraErrorCreate (NULL, description, module, file, line);
			error->codeFromKey = elektraStrDup (codeFromKey);
			error->errorKey = key;

			error->warnings[i] = warning;
		}
	}
	else
	{
		error->warningCount = 0;
		error->warnings = NULL;
	}

	return error;
}

/**
 * Creates a "Key not found" error
 *
 * @param keyname The name of the key that wasn't found.
 *
 * @return A newly allocated ElektraError (free with elektraErrorReset()).
 */
ElektraError * elektraErrorKeyNotFound (const char * keyname)
{
	char * description = elektraFormat ("The key '%s' could not be found.", keyname);
	ElektraError * error = elektraErrorCreate (ELEKTRA_ERROR_INTERNAL, description, "highlevel", "unknown", 0);
	elektraFree (description);
	return error;
}

/**
 * Creates a "Wrong type" error
 *
 * @param keyname      The name of the key that had the wrong type.
 * @param expectedType The type that was expected.
 * @param actualType   The type that was actually found.
 *
 * @return A newly allocated ElektraError (free with elektraErrorReset()).
 */
ElektraError * elektraErrorWrongType (const char * keyname, KDBType expectedType, KDBType actualType)
{
	char * description =
		elektraFormat ("The key '%s' has the wrong type (expected '%s' but got '%s').", keyname, expectedType, actualType);
	ElektraError * error = elektraErrorCreate (ELEKTRA_ERROR_VALIDATION_SEMANTIC, description, "highlevel", "unknown", 0);
	elektraFree (description);
	return error;
}

/**
 * Creates a "Null error argument" error
 *
 * @param function The name of the function that was called with a null pointer error argument.
 *
 * @return A newly allocated ElektraError (free with elektraErrorReset()).
 */
ElektraError * elektraErrorNullError (const char * function)
{
	char * description = elektraFormat ("The value passed to the ElektraError ** argument of %s was NULL.", function);
	ElektraError * error = elektraErrorCreate (ELEKTRA_ERROR_INTERNAL, description, "highlevel", "unknown", 0);
	elektraFree (description);
	return error;
}

/**
 * Creates a "Conversion to string failed" error
 *
 * @param sourceType The type which failed to be converted to string.
 * @param keyname    The name of the key that couldn't be converted.
 *
 * @return A newly allocated ElektraError (free with elektraErrorReset()).
 */
ElektraError * elektraErrorConversionToString (KDBType sourceType, const char * keyname)
{
	char * description = elektraFormat ("The value of key '%s' with type '%s' could not be converted to string.", keyname, sourceType);
	ElektraError * error = elektraErrorCreate (ELEKTRA_ERROR_VALIDATION_SEMANTIC, description, "highlevel", "unknown", 0);
	elektraFree (description);
	return error;
}

/**
 * Creates a "Conversion from string failed" error
 *
 * @param targetType  The type into which @p sourceValue couldn't be converted.
 * @param keyname     The name of the key that couldn't be converted.
 * @param sourceValue The value that couldn't be converted.
 *
 * @return A newly allocated ElektraError (free with elektraErrorReset()).
 */
ElektraError * elektraErrorConversionFromString (KDBType targetType, const char * keyname, const char * sourceValue)
{
	char * description =
		elektraFormat ("The value '%s' of key '%s' could not be converted to type '%s'.", sourceValue, keyname, targetType);
	ElektraError * error = elektraErrorCreate (ELEKTRA_ERROR_VALIDATION_SEMANTIC, description, "highlevel", "unknown", 0);
	elektraFree (description);
	return error;
}

// elektra/error.h

/**
 * \addtogroup highlevel High-level API
 * @{
 */

/**
 * Creates a dummy ElektraError struct to store warnings in.
 * If elektraErrorCode() is called on the resulting struct, it will return NULL.
 *
 * @return A newly allocated ElektraError (free with elektraFree()).
 */
ElektraError * elektraErrorPureWarning (void)
{
	return elektraErrorCreate (NULL, "", NULL, NULL, -1);
}

/**
 * @return the error code of the given error
 */
const char * elektraErrorCode (const ElektraError * error)
{
	return error->errorKey == NULL ? error->code : error->codeFromKey;
}

/**
 * @return the description for the given error
 */
const char * elektraErrorDescription (const ElektraError * error)
{
	return error->description;
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

	if (actualError->codeFromKey != NULL)
	{
		elektraFree (actualError->codeFromKey);
	}

	if (actualError->warnings != NULL)
	{
		for (int i = 0; i < actualError->warningCount; ++i)
		{
			elektraErrorReset (&actualError->warnings[i]);
		}
		elektraFree (actualError->warnings);
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

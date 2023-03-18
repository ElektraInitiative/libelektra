/**
 * @file
 *
 * @brief The error module of the High level API.
 * Can be used to create errors from scratch or from errors that were attached to keys using src/libs/elektra/errors.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <elektra/highlevel/errors.h>
#include <elektra/kdb/errors.h>
#include <elektra/type/conversion.h>
#include <internal/kdbprivate.h>
#include <internal/utility/old_helper.h>
#include <string.h>

#ifdef __cplusplus
extern "C" {
#endif

// kdbprivate.h

/**
 * Creates a new ElektraError using the provided values.
 * The returned value will be allocated with elektraCalloc().
 *
 * @param code        The error code of the error. Will be copied and stored in the struct.
 * @param description The description of the error. Will be copied and stored in the struct.
 * @param module      The module that raised the error. Will be copied and stored in the struct.
 * @param file        The file that raised the error. Will be copied and stored in the struct.
 * @param line        The line in which the error was raised.
 *
 * @return A newly allocated ElektraError (free with elektraErrorReset()).
 */
ElektraError * elektraErrorCreate (const char * code, const char * description, const char * module, const char * file, kdb_long_t line)
{
	ElektraError * const error = elektraCalloc (sizeof (struct _ElektraError));
	error->code = code == NULL ? NULL : elektraStrDup (code);
	error->codeFromKey = NULL;
	error->description = description == NULL ? NULL : elektraStrDup (description);
	error->module = module == NULL ? NULL : elektraStrDup (module);
	error->file = file == NULL ? NULL : elektraStrDup (file);
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
 * @note Use the functions in src/libs/elektra/errors.c to add errors to a key.
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

		const char * codeFromKey = elektraStrDup (keyString (keyGetMeta (key, "error/number")));
		const char * description = elektraStrDup (keyString (keyGetMeta (key, "error/description")));
		const char * module = elektraStrDup (keyString (keyGetMeta (key, "error/module")));
		const char * file = elektraStrDup (keyString (keyGetMeta (key, "error/file")));

		char * fullDescription =
			reasonMeta != NULL ? elektraFormat ("%s: %s", description, keyString (reasonMeta)) : elektraStrDup (description);

		kdb_long_t line = 0;
		elektraKeyToLong (key, &line);
		error = elektraErrorCreate (NULL, fullDescription, module, file, line);
		error->codeFromKey = elektraStrDup (codeFromKey);
		error->errorKey = key;

		elektraFree (fullDescription);
	}


	// Code for extracting warnings was adapted from src/tools/kdb/coloredkdbio.h:printWarnings()
	KeySet * metaKeys = keyMeta (key);
	Key * warningsParent = keyNew ("meta:/warnings", KEY_END);
	KeySet * warningKeys = ksCut (metaKeys, warningsParent);
	if (ksGetSize (warningKeys) > 0)
	{
		for (elektraCursor it = 0; it < ksGetSize (warningKeys); it++)
		{
			if (!keyIsDirectlyBelow (warningsParent, ksAtCursor (warningKeys, it)))
			{
				// Warning details are sub-keys of the warning key. (e.g. .../warnings/#0/line, .../warnings/#0/reason, ...)
				// For the extraction to work, we have to ignore the warningsParent itself and only look at keys directly
				// below the warningParent (i.e. .../warnings/#0, .../warnings/#1, ...)
				continue;
			}

			// Extract warning details set by errors.c

			Key * warningKey = ksAtCursor (warningKeys, it);
			const char * warningKeyName = keyName (warningKey);

			char * lookupName = elektraFormat ("%s/number", warningKeyName);
			// "number" and "code" are used interchangeably for the error code. "line" is the line number.
			const char * code = keyString (ksLookupByName (warningKeys, lookupName, 0));
			elektraFree (lookupName);

			lookupName = elektraFormat ("%s/reason", warningKeyName);
			const Key * reasonKey = ksLookupByName (warningKeys, lookupName, 0);
			elektraFree (lookupName);

			lookupName = elektraFormat ("%s/description", warningKeyName);
			const char * description = keyString (ksLookupByName (warningKeys, lookupName, 0));
			elektraFree (lookupName);

			lookupName = elektraFormat ("%s/module", warningKeyName);
			const char * module = keyString (ksLookupByName (warningKeys, lookupName, 0));
			elektraFree (lookupName);

			lookupName = elektraFormat ("%s/file", warningKeyName);
			const char * file = keyString (ksLookupByName (warningKeys, lookupName, 0));
			elektraFree (lookupName);

			lookupName = elektraFormat ("%s/line", warningKeyName);
			Key * lineKey = ksLookupByName (warningKeys, lookupName, 0);
			elektraFree (lookupName);
			kdb_long_t lineNumber = -1;
			elektraKeyToLong (lineKey, &lineNumber);

			// Generate fullDescription out of reason and description. Reason might be null.
			char * fullDescription = reasonKey != NULL ? elektraFormat ("%s: %s", description, keyString (reasonKey)) :
								     elektraStrDup (description);

			// Code, module, file and lineNumber are compile-time constants, no need to strDup().
			ElektraError * warning = elektraErrorCreate (code, fullDescription, module, file, lineNumber);
			elektraFree (fullDescription);
			warning->codeFromKey = elektraStrDup (code);
			warning->errorKey = key;

			elektraErrorAddWarning (error, warning);
		}
	}

	keyDel (warningsParent);
	ksDel (warningKeys);

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

	if (actualError->code != NULL)
	{
		elektraFree (actualError->code);
	}

	if (actualError->module != NULL)
	{
		elektraFree (actualError->module);
	}

	if (actualError->file != NULL)
	{
		elektraFree (actualError->file);
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

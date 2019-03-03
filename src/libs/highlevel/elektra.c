/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra.h"
#include "elektra/conversion.h"
#include "elektra/errorsprivate.h"
#include "elektra/types.h"
#include "kdbhelper.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

static void defaultFatalErrorHandler (ElektraError * error)
{
	ELEKTRA_LOG_DEBUG ("FATAL ERROR [%d]: %s", error->code, error->description);
	ElektraErrorCode code = error->code;
	elektraFree (error);
	exit (code);
}

static struct _ElektraKDBError * elektraKDBErrorFromKey (Key * key);
static ElektraError * elektraErrorCreateFromKey (Key * key);
static ElektraError * elektraErrorWarningFromKey (Key * key);

/**
 * \defgroup highlevel High-level API
 * @{
 */

/**
 * Initializes a new Elektra instance.
 *
 * To free the memory allocated by this function call elektraClose(),
 * once you are done using this instance.
 *
 * @param application 	Your application's base name. The the simplest version for this string is
 * 			"/sw/org/<appname>/#0/current", where '<appname>' is a unique name for
 * 			your application. For more information see the man-page elektra-key-names(7).
 * @param defaults	A KeySet containing default values. If you pass NULL, trying to read
 * 			a non-existent value will cause a fatal error. It is recommended, to
 * 			only pass NULL, if you are using a specification, which provides
 * 			default values inside of the KDB.
 * @param error		If an error occurs during initialization of the Elektra instance, this pointer
 * 			will be used to report the error.
 *
 * @return An Elektra instance initialized with the application.
 *
 * @see elektraClose
 */
Elektra * elektraOpen (const char * application, KeySet * defaults, ElektraError ** error)
{
	Key * const parentKey = keyNew (application, KEY_END);
	KDB * const kdb = kdbOpen (parentKey);

	if (kdb == NULL)
	{
		*error = elektraErrorCreateFromKey (parentKey);
		return NULL;
	}

	KeySet * const config = ksNew (0, KS_END);
	if (defaults != NULL)
	{
		ksRewind (defaults);
		for (Key * key = ksNext (defaults); key != NULL; key = ksNext (defaults))
		{
			Key * const dup = keyDup (key);
			const char * name = keyName (key);
			keySetName (dup, keyName (parentKey));
			keyAddName (dup, name);
			ksAppendKey (config, dup);
		}
	}

	const int kdbGetResult = kdbGet (kdb, config, parentKey);

	if (kdbGetResult == -1)
	{
		*error = elektraErrorCreateFromKey (parentKey);
		return NULL;
	}

	Elektra * const elektra = elektraCalloc (sizeof (struct _Elektra));
	elektra->kdb = kdb;
	elektra->parentKey = parentKey;
	elektra->config = config;
	elektra->lookupKey = keyNew (NULL, KEY_END);
	elektra->fatalErrorHandler = &defaultFatalErrorHandler;

	return elektra;
}

/**
 * Promote an ElektraError to fatal and call the fatal error handler.
 *
 * @param elektra    Elektra instance whose fatal error handler shall be used.
 * @param fatalError The error that will be raised.
 */
void elektraFatalError (Elektra * elektra, ElektraError * fatalError)
{
	fatalError->severity = ELEKTRA_ERROR_SEVERITY_FATAL;
	elektra->fatalErrorHandler (fatalError);
}

/**
 * Sets the fatal error handler that will be called, whenever a fatal error occurs.
 *
 * Errors occurring in a function, which does not take a pointer to ElektraError,
 * are always considered fatal.
 *
 * If this function returns, i.e. it does not call exit() or interrupt the thread of
 * execution in some other way, the behaviour of the function from which the error
 * originated is generally undefined.
 *
 * @param elektra           An Elektra instance.
 * @param fatalErrorHandler The error handler that will be used henceforth.
 */
void elektraFatalErrorHandler (Elektra * elektra, ElektraErrorHandler fatalErrorHandler)
{
	elektra->fatalErrorHandler = fatalErrorHandler;
}

/**
 * Releases all resources used by the given elektra instance. The elektra instance must not be used anymore after calling this.
 * @param elektra An Elektra instance.
 */
void elektraClose (Elektra * elektra)
{
	kdbClose (elektra->kdb, elektra->parentKey);
	keyDel (elektra->parentKey);
	ksDel (elektra->config);
	keyDel (elektra->lookupKey);

	elektraFree (elektra);
}

/**
 * @}
 */

// Private definitions

KDBType KDB_TYPE_STRING = "string";
KDBType KDB_TYPE_BOOLEAN = "boolean";
KDBType KDB_TYPE_CHAR = "char";
KDBType KDB_TYPE_OCTET = "octet";
KDBType KDB_TYPE_SHORT = "short";
KDBType KDB_TYPE_UNSIGNED_SHORT = "unsigned_short";
KDBType KDB_TYPE_LONG = "long";
KDBType KDB_TYPE_UNSIGNED_LONG = "unsigned_long";
KDBType KDB_TYPE_LONG_LONG = "long_long";
KDBType KDB_TYPE_UNSIGNED_LONG_LONG = "unsigned_long_long";
KDBType KDB_TYPE_FLOAT = "float";
KDBType KDB_TYPE_LONG_DOUBLE = "long_double";
KDBType KDB_TYPE_DOUBLE = "double";
KDBType KDB_TYPE_ENUM = "enum";

void elektraSetLookupKey (Elektra * elektra, const char * name)
{
	keySetName (elektra->lookupKey, keyName (elektra->parentKey));
	keyAddName (elektra->lookupKey, name);
}

void elektraSetArrayLookupKey (Elektra * elektra, const char * name, kdb_long_long_t index)
{
	elektraSetLookupKey (elektra, name);
	char arrayPart[ELEKTRA_MAX_ARRAY_SIZE];
	elektraWriteArrayNumber (arrayPart, index);
	keyAddName (elektra->lookupKey, arrayPart);
}

void elektraSaveKey (Elektra * elektra, Key * key, ElektraError ** error)
{
	int ret = 0;
	do
	{
		ksAppendKey (elektra->config, key);

		ret = kdbSet (elektra->kdb, elektra->config, elektra->parentKey);
		if (ret == -1)
		{
			ElektraError * kdbSetError = elektraErrorCreateFromKey (elektra->parentKey);
			if (elektraErrorCode (kdbSetError) != ELEKTRA_ERROR_CODE_LOW_LEVEL)
			{
				*error = kdbSetError;
				return;
			}

			if (elektraKDBErrorCode (kdbSetError) != 30) // ELEKTRA_ERROR_CONFLICT = 30
			{
				*error = kdbSetError;
				return;
			}

			elektraErrorReset (&kdbSetError);

			Key * problemKey = ksCurrent (elektra->config);
			if (problemKey != NULL)
			{
				ELEKTRA_LOG_DEBUG ("problemKey: %s\n", keyName (problemKey));
			}

			key = keyDup (key);
			kdbGet (elektra->kdb, elektra->config, elektra->parentKey);
		}
	} while (ret == -1);
}

/**
 * Creates a new ElektraError by using the values of the error/warning metadata of a Key.
 *
 * @param Key The key from which the error data shall be taken.
 * @return A new ElektraError created with elektraErrorCreate().
 */
static ElektraError * elektraErrorCreateFromKey (Key * key)
{
	const Key * metaKey = keyGetMeta (key, "error");

	if (NULL == metaKey)
	{
		return elektraErrorWarningFromKey (key);
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

static ElektraError * elektraErrorWarningFromKey (Key * key)
{
	kdb_long_t warningCount = 0;
	const Key * warningsKey = keyGetMeta (key, "warnings");
	if (warningsKey != NULL)
	{
		elektraKeyToLong (warningsKey, &warningCount);
	}

	if (warningCount < 1)
	{
		return NULL;
	}

	ElektraError * error;
	error = elektraErrorLowLevel (ELEKTRA_ERROR_SEVERITY_WARNING, -1, "One or more warnings were found.", "", "");
	error->lowLevelError = elektraCalloc (sizeof (struct _ElektraKDBError));

	error->lowLevelError->code = -1;
	error->lowLevelError->description = "One or more warnings were found.";
	error->lowLevelError->severity = ELEKTRA_ERROR_SEVERITY_WARNING;
	error->lowLevelError->group = "";
	error->lowLevelError->module = "";
	error->lowLevelError->reason = "";
	error->lowLevelError->errorKey = key;

	error->lowLevelError->warningCount = warningCount;

	struct _ElektraKDBError ** warnings = elektraCalloc (warningCount * sizeof (struct _ElektraKDBError *));

	for (int i = 0; i < warningCount; ++i)
	{
		struct _ElektraKDBError * const warning = elektraCalloc (sizeof (struct _ElektraKDBError));
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
	error->lowLevelError->warnings = warnings;


	return error;
}


static struct _ElektraKDBError * elektraKDBErrorFromKey (Key * key)
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

	struct _ElektraKDBError * const error = elektraCalloc (sizeof (struct _ElektraKDBError));
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
		struct _ElektraKDBError ** warnings = elektraCalloc (warningCount * sizeof (struct _ElektraKDBError *));

		for (int i = 0; i < warningCount; ++i)
		{
			struct _ElektraKDBError * const warning = elektraCalloc (sizeof (struct _ElektraKDBError));
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

#ifdef __cplusplus
};
#endif

/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra.h"
#include "elektra/conversion.h"
#include "elektra/errors.h"
#include "elektra/types.h"
#include "kdberrors.h"
#include "kdbhelper.h"
#include "kdblogger.h"
#include "kdbprivate.h"
#include <stdlib.h>

#ifdef __cplusplus
extern "C" {
#endif

static void defaultFatalErrorHandler (ElektraError * error)
{
	ELEKTRA_LOG_DEBUG ("FATAL ERROR [%s]: %s", error->code, error->description);
	elektraFree (error);
	exit (EXIT_FAILURE);
}

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
		*error = elektraErrorFromKey (parentKey);
		return NULL;
	}

	KeySet * const config = ksNew (0, KS_END);
	insertDefaults (config, parentKey, defaults);

	const int kdbGetResult = kdbGet (kdb, config, parentKey);

	if (kdbGetResult == -1)
	{
		*error = elektraErrorFromKey (parentKey);
		return NULL;
	}

	Elektra * const elektra = elektraCalloc (sizeof (struct _Elektra));
	elektra->kdb = kdb;
	elektra->parentKey = parentKey;
	elektra->parentKeyLength = keyGetNameSize (parentKey) - 1;
	elektra->config = config;
	elektra->lookupKey = keyNew (NULL, KEY_END);
	elektra->fatalErrorHandler = &defaultFatalErrorHandler;
	elektra->defaults = ksDup (defaults);

	return elektra;
}

/**
 * Calls kdbEnsure() on the internal KDB handle of this Elektra instance.
 *
 * @param elektra  Elektra instance to use.
 * @param contract The contract to ensure.
 * @param error    Pass a reference to an ElektraError pointer.
 *                 Will only be set in case of an error.
 *
 * @see kdbEnsure()
 */
void elektraEnsure (Elektra * elektra, KeySet * contract, ElektraError ** error)
{
	if (error == NULL)
	{
		elektraFatalError (elektra, elektraErrorNullError (__func__));
		return;
	}

	Key * parentKey = keyDup (elektra->parentKey);

	kdbClose (elektra->kdb, parentKey);
	ksClear (elektra->config);
	KDB * const kdb = kdbOpen (parentKey);

	if (kdb == NULL)
	{
		*error = elektraErrorFromKey (parentKey);
		return;
	}

	int rc = kdbEnsure (kdb, contract, parentKey);
	if (rc == 0)
	{
		// if successful, refresh config
		elektra->kdb = kdb;
		insertDefaults (elektra->config, elektra->parentKey, elektra->defaults);
		const int kdbGetResult = kdbGet (elektra->kdb, elektra->config, parentKey);

		if (kdbGetResult == -1)
		{
			*error = elektraErrorFromKey (parentKey);
			return;
		}
	}
	else if (rc == 1)
	{
		const char * reason = keyString (keyGetMeta (parentKey, "error/reason"));
		*error = elektraErrorEnsureFailed (reason);
	}
	else
	{
		*error = elektraErrorFromKey (parentKey);
	}

	keyDel (parentKey);
}

/**
 * Promote an ElektraError to fatal and call the fatal error handler.
 *
 * @param elektra    Elektra instance whose fatal error handler shall be used.
 * @param fatalError The error that will be raised.
 */
void elektraFatalError (Elektra * elektra, ElektraError * fatalError)
{
	elektra->fatalErrorHandler (fatalError);
}

/**
 * This function is only intended for use with code-generation.
 *
 * It looks for the key proc/elektra/gopts/help (absolute name) created by gopts,
 * and returns it if found.
 *
 * @param elektra The Elektra instance to check
 *
 * @return the help key if found, NULL otherwise
 */
Key * elektraHelpKey (Elektra * elektra)
{
	return ksLookupByName (elektra->config, "proc/elektra/gopts/help", 0);
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

	if (elektra->resolvedReference != NULL)
	{
		elektraFree (elektra->resolvedReference);
	}

	if (elektra->defaults != NULL)
	{
		ksDel (elektra->defaults);
	}

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
			ElektraError * kdbSetError = elektraErrorFromKey (elektra->parentKey);
			if (strcmp (elektraErrorCode (kdbSetError), ELEKTRA_ERROR_CONFLICTING_STATE) != 0)
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

#ifdef __cplusplus
};
#endif

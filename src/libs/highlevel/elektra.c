/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra.h"
#include "elektra/conversion.h"
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
	elektraErrorReset (&error);
	exit (EXIT_FAILURE);
}

static void insertDefaults (ElektraKeyset * config, const ElektraKey * parentKey, ElektraKeyset * defaults);

static kdb_boolean_t checkSpecProperlyMounted (ElektraKdb * const kdb, const char * application, ElektraError ** error);
static kdb_boolean_t checkSpecificationMountPoint (ElektraKeyset * const mountPointsKs, const char * application, const char * mountPoint,
						   ElektraError ** error);
static kdb_boolean_t checkSpecToken (ElektraKdb * const kdb, ElektraKey * parentKey, const char * tokenFromContract, ElektraError ** error);
static char * generateSpecProblemErrorMessage (const char * application);

/**
 * \defgroup highlevel High-level API
 * @{
 */

kdb_boolean_t checkSpec (ElektraKey * const parentKey, ElektraKeyset * contract, ElektraError ** error);
/**
 * Initializes a new Elektra instance.
 *
 * To free the memory allocated by this function call elektraClose(),
 * once you are done using this instance.
 *
 * @param application 	Your application's base name. The simplest version for this string is
 * 			"/sw/org/<appname>/#0/current", where '<appname>' is a unique name for
 * 			your application. For more information see the man-page elektra-key-names(7).
 * @param defaults	A KeySet containing default values. If you pass NULL, trying to read
 * 			a non-existent value will cause a fatal error. It is recommended, to
 * 			only pass NULL, if you are using a specification, which provides
 * 			default values inside of the KDB.
 * 			If a key in this KeySet doesn't have a value, we will use the value of the "default"
 * 			metakey of this key.
 * @param contract      Will be passed to kdbOpen() as the contract.
 * @param error		If an error occurs during initialization of the Elektra instance, this pointer
 * 			will be used to report the error.
 *
 * @return An Elektra instance initialized for the application (free with elektraClose()).
 *
 * @see elektraClose
 * @see kdbOpen
 */
Elektra * elektraOpen (const char * application, ElektraKeyset * defaults, ElektraKeyset * contract, ElektraError ** error)
{
	ElektraKey * const parentKey = elektraKeyNew (application, ELEKTRA_KEY_END);

	// Before anything else: Verify that the specification is okay.
	if (!checkSpec (parentKey, contract, error))
	{
		elektraKeyDel (parentKey);
		return NULL;
	}

	ElektraKdb * const kdb = elektraKdbOpen (contract, parentKey);

	if (kdb == NULL)
	{
		*error = elektraErrorFromKey (parentKey);
		elektraKeyDel (parentKey);
		return NULL;
	}

	int ignoreRequireInHelpMode = 0;

	if (contract != NULL)
	{
		// TODO: set default spec config to use ERROR
		elektraKeysetAppendKey (contract, elektraKeyNew ("system:/elektra/contract/mountglobal/spec", ELEKTRA_KEY_END));
		elektraKeysetAppendKey (contract,
			     elektraKeyNew ("system:/elektra/contract/mountglobal/spec/config/conflict/get", ELEKTRA_KEY_VALUE, "ERROR", ELEKTRA_KEY_END));
		elektraKeysetAppendKey (contract,
			     elektraKeyNew ("system:/elektra/contract/mountglobal/spec/config/conflict/set", ELEKTRA_KEY_VALUE, "ERROR", ELEKTRA_KEY_END));
		elektraKeysetAppendKey (contract, elektraKeyNew ("system:/elektra/contract/mountglobal/spec/config/missing/log", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END));

		ElektraKey * contractCut = elektraKeyNew ("system:/elektra/contract/highlevel", ELEKTRA_KEY_END);
		ElektraKeyset * highlevelContract = elektraKeysetCut (contract, contractCut);

		if (elektraKeysetGetSize (highlevelContract) > 0)
		{
			elektraKeysetAppend (contract, highlevelContract);
			if (elektraKeysetLookupByName (highlevelContract, "system:/elektra/contract/highlevel/helpmode/ignore/require", 0) != NULL)
			{
				ignoreRequireInHelpMode = 1;
			}
		}

		elektraKeyDel (contractCut);
		elektraKeysetDel (highlevelContract);
	}

	ElektraKeyset * const config = elektraKeysetNew (0, ELEKTRA_KS_END);
	if (defaults != NULL)
	{
		insertDefaults (config, parentKey, defaults);
	}

	const int kdbGetResult = elektraKdbGet (kdb, config, parentKey);
	// Note: if kdbGetResult is not -1, config now contains the defaults passed to this function and any keys that were found during
	// kdbGet().

	// If a warning occurs in kdbGet, it will not return -1. We need to check the parentKey for warnings.
	if (kdbGetResult >= 0)
	{
		// Applications using the HL API should treat warnings as errors. Therefore, if a warning occurred, set the error param and
		// return NULL.
		ElektraError * errorFromKdbGet = elektraErrorFromKey (parentKey);
		if (errorFromKdbGet->warningCount > 0)
		{
			// If there are warnings, pick the first warning, create a copy, set it to param "error" and return NULL.
			// We can only report 1 error at a time. Once the user has fixed that error, they will be informed about the next
			// one on the next execution of the application.
			*error = elektraErrorCreate (errorFromKdbGet->warnings[0]->code, errorFromKdbGet->warnings[0]->description,
						     errorFromKdbGet->warnings[0]->module, errorFromKdbGet->warnings[0]->file,
						     errorFromKdbGet->warnings[0]->line);

			elektraErrorReset (&errorFromKdbGet);
			elektraKeysetDel (config);
			elektraKdbClose (kdb, parentKey);
			elektraKeyDel (parentKey);
			return NULL;
		}
		elektraErrorReset (&errorFromKdbGet);
	}
	// If kdbGet() returns -1, there was an error.
	else if (kdbGetResult == -1)
	{
		ElektraKey * helpKey = elektraKeysetLookupByName (config, "proc:/elektra/gopts/help", 0);
		const ElektraKey * missingKeys = elektraKeyGetMeta (parentKey, "logs/spec/missing");
		if (ignoreRequireInHelpMode == 1 && helpKey != NULL && missingKeys != NULL)
		{
			// proc:/elektra/gopts/help was set -> we know we are in help mode
			// logs/spec/missing exists on parentKey -> we know that spec detected missing keys
			// we ensured that spec uses conflict/get = ERROR -> we know that the error in kdbGet must be from spec
			// --> Therefore, we are in the error case that should be ignored

			// BUT: anything other than helpKey may be incorrect
			// and only helpKey should be used anyway
			// so create a new config KeySet
			ElektraKey * helpKeyDup = elektraKeyDup (helpKey, ELEKTRA_KEY_CP_ALL);
			elektraKeysetClear (config);
			elektraKeysetAppendKey (config, helpKeyDup);
		}
		else
		{
			// We are in the error case that should not be ignored.
			*error = elektraErrorFromKey (parentKey);

			elektraKeysetDel (config);
			elektraKdbClose (kdb, parentKey);
			elektraKeyDel (parentKey);
			return NULL;
		}
	}

	Elektra * const elektra = elektraCalloc (sizeof (struct _Elektra));
	elektra->kdb = kdb;
	elektra->parentKey = parentKey;
	elektra->parentKeyLength = elektraKeyGetNameSize (parentKey) - 1;
	elektra->config = config;
	elektra->lookupKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	elektra->fatalErrorHandler = &defaultFatalErrorHandler;
	elektra->defaults = elektraKeysetDup (defaults);

	return elektra;
}

/**
 * Verify that specification is properly mounted and is equal to specification at compile time.
 * Note: These checks are only executed, if the contract requires them.
 *
 * @param parentKey The parentKey of the application
 * @param contract The contract passed to HL API by the application.
 * @param error Pointer used to report errors
 * @retval True if the checks were successful (or not required).
 * @retval False if the checks were required but unsuccessful.
 */
kdb_boolean_t checkSpec (ElektraKey * const parentKey, ElektraKeyset * contract, ElektraError ** error)
{
	if (contract != NULL)
	{
		// Execute kdbOpen(), but do not pass the contract!
		// Reason: The contract may contain requirements that interfere with the checks below.
		// E.g., If the contract contains keys from elektraGOptsContract(), kdbGet() might fail, because the specification is not
		// mounted or incorrect. Therefore, we first have to check, that the spec is properly mounted and hasn't been modified since
		// compilation.
		ElektraKdb * kdb = elektraKdbOpen (NULL, parentKey);

		if (kdb == NULL)
		{
			*error = elektraErrorFromKey (parentKey);
			return false;
		}

		// Verify that the specification is properly mounted, if contract requires it.
		ElektraKey * checkSpecFromContract = elektraKeysetLookupByName (contract, "system:/elektra/contract/highlevel/check/spec/mounted", 0);
		kdb_boolean_t shouldCheckSpecProperlyMounted = false;
		if (checkSpecFromContract != NULL && elektraKeyToBoolean (checkSpecFromContract, &shouldCheckSpecProperlyMounted) &&
		    shouldCheckSpecProperlyMounted)
		{
			// If the specification was not properly mounted, we don't return an Elektra instance.
			// Reason: the application won't function properly without a properly mounted specification.
			if (!checkSpecProperlyMounted (kdb, elektraKeyName (parentKey), error))
			{
				elektraKdbClose (kdb, parentKey);
				elektraKeyDel (checkSpecFromContract);
				return false;
			}

			// If the contract contains a specification token, verify that is is equal to the current specification token.
			ElektraKey * tokenFromContractKey = elektraKeysetLookupByName (contract, "system:/elektra/contract/highlevel/check/spec/token", 0);
			const char * tokenFromContract = NULL;
			if (tokenFromContractKey != NULL && elektraKeyToString (tokenFromContractKey, &tokenFromContract) &&
			    tokenFromContract != NULL && strlen (tokenFromContract) > 0)
			{
				if (!checkSpecToken (kdb, parentKey, tokenFromContract, error))
				{
					elektraKdbClose (kdb, parentKey);
					elektraKeyDel (checkSpecFromContract);
					elektraKeyDel (tokenFromContractKey);
					return false;
				}
			}
			elektraKeyDel (tokenFromContractKey);
		}
		elektraKeyDel (checkSpecFromContract);
		elektraKdbClose (kdb, parentKey);
	}
	return true;
}

/**
 * Check whether the specification (with which the application was compiled with) matches the current specification on the system.
 *
 * @param parentKey The parentKey of the application.
 * @param config The application's config.
 * @param tokenFromContract The token from the contract (= token from compilation).
 * @param error		Pointer used to report errors.
 * @retval true on success
 * @retval false on failure
 */
kdb_boolean_t checkSpecToken (ElektraKdb * const kdb, ElektraKey * parentKey, const char * tokenFromContract, ElektraError ** error)
{
	ElektraKeyset * const specificationKs = elektraKeysetNew (0, ELEKTRA_KS_END);

	ElektraKey * parentKeySpecNamespace = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	// For token calculation of an application using the HL API, only keys within the spec namespace are relevant.
	elektraKeySetNamespace (parentKeySpecNamespace, ELEKTRA_NS_SPEC);

	const int kdbGetResult = elektraKdbGet (kdb, specificationKs, parentKeySpecNamespace);
	if (kdbGetResult == -1)
	{
		elektraKeysetDel (specificationKs);
		*error = elektraErrorFromKey (parentKeySpecNamespace);
		return false;
	}
	else
	{
		char calculatedToken[65];
		kdb_boolean_t success = calculateSpecificationToken (calculatedToken, specificationKs, parentKeySpecNamespace);

		// If the token calculation failed, don't return an Elektra instance.
		if (!success)
		{
			elektraKeysetDel (specificationKs);
			*error = elektraErrorFromKey (parentKeySpecNamespace);
			elektraKeyDel (parentKeySpecNamespace);
			return false;
		}

		// If tokens aren't equal, report an error and fail
		if (strcmp (tokenFromContract, calculatedToken) != 0)
		{
			char * errorMessage = generateSpecProblemErrorMessage (elektraKeyName (parentKey));
			char * description = elektraFormat (
				"%s\n"
				"Technical details: The configuration specification on your system was modified after installation.\n"
				"The token was \"%s\" during compilation\nbut now it's \"%s\"\n",
				errorMessage, tokenFromContract, calculatedToken);
			elektraFree (errorMessage);
			*error = elektraErrorCreate (ELEKTRA_ERROR_VALIDATION_SEMANTIC, description, "highlevel", "unknown", 0);
			elektraFree (description);
			elektraKeysetDel (specificationKs);
			elektraKeyDel (parentKeySpecNamespace);
			return false;
		}
	}
	return true;
}

Elektra * ELEKTRA_SYMVER (elektraOpen, v1) (const char * application, ElektraKeyset * defaults, ElektraError ** error)
{
	return elektraOpen (application, defaults, NULL, error);
}

ELEKTRA_SYMVER_DECLARE ("libelektra_0.8", elektraOpen, v1)

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
 * Generate a error message about a problem with the specification.
 *
 * @note:  The returned char array needs to be freed with elektraFree() after usage.
 *
 * @param application The application's name.
 * @return Pointer to a char. Needs to be freed using elektraFree() after usage.
 */
static char * generateSpecProblemErrorMessage (const char * application)
{
	return elektraFormat (
		"There was a problem with the application's specification. \n\nTo fix this, execute:\n"
		"\t\"$ sudo kdb rm -r spec:%s\"\n"
		"\t\"$ sudo kdb umount %s\"\n"
		"\t\"$ sudo kdb umount spec:%s\"\n"
		"and then reinstall the application.\n\n"
		"If that does not help, please consult the application's documentation or contact its developers.\n",
		application, application, application);
}

/**
 *
 * Check whether the specification for @p application was properly mounted using "kdb mount" and "kdb spec-mount".
 *
 * There is currently no way to check this with full certainty.
 * Therefore, the following best-effort heuristic is used:
 * (for details see https://github.com/ElektraInitiative/libelektra/issues/3998)
 *
 * "kdb mount" was properly executed if:
 * 1. Key "system:/elektra/mountpoints/spec:ESCAPED_APPLICATION_NAME/mountpoint" exists
 * 2. its value matches "spec:/APPLICATION_NAME"
 *
 * "kdb spec-mount" was properly executed if:
 * 1. Key "system:/elektra/mountpoints/ESCAPED_APPLICATION_NAME/mountpoint" exists
 * 2. its value matches "APPLICATION_NAME"
 *
 * @param kdb		The KDB instance used for checking.
 * @param application 	The application's base name.
 * @param error		Pointer used to report errors.
 * @return True if the specification file was properly mounted, false otherwise.
 */
static kdb_boolean_t checkSpecProperlyMounted (ElektraKdb * const kdb, const char * application, ElektraError ** error)
{
	ElektraKeyset * const mountPoints = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * const parentKey = elektraKeyNew ("system:/elektra/mountpoints", ELEKTRA_KEY_END);
	const int kdbGetResult = elektraKdbGet (kdb, mountPoints, parentKey);

	if (kdbGetResult == -1)
	{
		elektraKeysetDel (mountPoints);
		*error = elektraErrorFromKey (parentKey);
		elektraKeyDel (parentKey);
		return false;
	}
	else
	{
		kdb_boolean_t success = true;
		// Construct the mount point used by "kdb mount".
		char * kdbMountMountPoint = elektraFormat ("spec:%s", application);
		// Check if "kdb mount" was properly executed.
		if (!checkSpecificationMountPoint (mountPoints, application, kdbMountMountPoint, error))
		{
			success = false;
		}
		// Note: the mount point used by "kdb spec-mount" does not have to be constructed, because it is equal to the param
		// "application". Check if "kdb spec-mount" was properly executed.
		else if (!checkSpecificationMountPoint (mountPoints, application, application, error))
		{
			success = false;
		}
		elektraKeysetDel (mountPoints);
		elektraKeyDel (parentKey);
		elektraFree (kdbMountMountPoint);
		return success;
	}
}

/**
 * Check whether the given mount point exists and has the expected value.
 *
 * @param mountPointsKs		The KeySet to use for ksLookup().
 * @param application 		The application's base name.
 * @param mountPoint 		The mount point to check.
 * @param error 		Pointer used to report errors.
 * @return True on success, false otherwise.
 */
static kdb_boolean_t checkSpecificationMountPoint (ElektraKeyset * const mountPointsKs, const char * application, const char * mountPoint,
						   ElektraError ** error)
{
	// TODO (kodebach): update check
	// Construct the lookup key
	ElektraKey * mountPointLookupKey = elektraKeyNew ("system:/elektra/mountpoints/", ELEKTRA_KEY_END);
	elektraKeyAddBaseName (mountPointLookupKey, mountPoint);
	elektraKeyAddBaseName (mountPointLookupKey, "mountpoint");

	ElektraKey * mountPointKey = elektraKeysetLookup (mountPointsKs, mountPointLookupKey, 0);
	// If the mountPointKey does not exist, the specification was not properly mounted.
	if (mountPointKey == NULL)
	{
		char * errorMessage = generateSpecProblemErrorMessage (application);
		char * description = elektraFormat (
			"%s\n"
			"Technical details: \n"
			"The mountPointKey \"%s\" should exist, but it does not.\n"
			"This was likely caused by an incomplete installation of the application.\n",
			errorMessage, elektraKeyName (mountPointLookupKey));
		elektraFree (errorMessage);
		elektraKeyDel (mountPointLookupKey);
		*error = elektraErrorCreate (ELEKTRA_ERROR_INSTALLATION, description, "elektra", "unknown", 0);
		elektraFree (description);
		return false;
	}
	// If the mountPointKey's value is not equal to "application", the specification was not properly mounted.
	else if (elektraStrCmp (elektraKeyString (mountPointKey), mountPoint) != 0)
	{
		char * errorMessage = generateSpecProblemErrorMessage (application);
		char * description = elektraFormat (
			"%s\n"
			"Technical details: \n"
			"The value of key \"%s\" should be \"%s\" but it is \"%s\".\n"
			"This was likely caused by an incomplete installation of the application.\n",
			errorMessage, elektraKeyName (mountPointKey), mountPoint, elektraKeyString (mountPointKey));
		elektraFree (errorMessage);
		*error = elektraErrorCreate (ELEKTRA_ERROR_INSTALLATION, description, "elektra", "unknown", 0);
		elektraKeyDel (mountPointLookupKey);
		elektraKeyDel (mountPointKey);
		elektraFree (description);
		return false;
	}
	// Both checks succeeded.
	else
	{
		elektraKeyDel (mountPointLookupKey);
		elektraKeyDel (mountPointKey);
		return true;
	}
}

/**
 * This function is only intended for use with code-generation.
 *
 * It looks for the key proc:/elektra/gopts/help (absolute name) created by gopts,
 * and returns it if found.
 *
 * @param elektra The Elektra instance to check
 *
 * @return the help key if found, NULL otherwise
 *   The pointer returned may become invalid, when any `elektraSet*()` function or
 *   any other function that modifies the state of @p elektra is called.
 *   It will always become invalid, when elektraClose() is called on @p elektra.
 */
ElektraKey * elektraHelpKey (Elektra * elektra)
{
	return elektraKeysetLookupByName (elektra->config, "proc:/elektra/gopts/help", 0);
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
	if (elektra == NULL)
	{
		return;
	}
	elektraKdbClose (elektra->kdb, elektra->parentKey);
	elektraKeyDel (elektra->parentKey);
	elektraKeysetDel (elektra->config);
	elektraKeyDel (elektra->lookupKey);

	if (elektra->resolvedReference != NULL)
	{
		elektraFree (elektra->resolvedReference);
	}

	if (elektra->defaults != NULL)
	{
		elektraKeysetDel (elektra->defaults);
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
	elektraKeySetName (elektra->lookupKey, elektraKeyName (elektra->parentKey));
	elektraKeyAddName (elektra->lookupKey, name);
}

void elektraSetArrayLookupKey (Elektra * elektra, const char * name, kdb_long_long_t index)
{
	elektraSetLookupKey (elektra, name);
	char arrayPart[ELEKTRA_MAX_ARRAY_SIZE];
	elektraWriteArrayNumber (arrayPart, index);
	elektraKeyAddName (elektra->lookupKey, arrayPart);
}

void elektraSaveKey (Elektra * elektra, ElektraKey * key, ElektraError ** error)
{
	int ret = 0;
	do
	{
		elektraKeysetAppendKey (elektra->config, key);

		ret = elektraKdbSet (elektra->kdb, elektra->config, elektra->parentKey);
		if (ret == -1)
		{
			ElektraError * kdbSetError = elektraErrorFromKey (elektra->parentKey);
			if (strcmp (elektraErrorCode (kdbSetError), ELEKTRA_ERROR_CONFLICTING_STATE) != 0)
			{
				*error = kdbSetError;
				return;
			}

			elektraErrorReset (&kdbSetError);

			ElektraKey * problemKey = elektraKeysetCurrent (elektra->config);
			if (problemKey != NULL)
			{
				ELEKTRA_LOG_DEBUG ("problemKey: %s\n", elektraKeyName (problemKey));
			}

			key = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
			elektraKdbGet (elektra->kdb, elektra->config, elektra->parentKey);
		}
	} while (ret == -1);
}

void insertDefaults (ElektraKeyset * config, const ElektraKey * parentKey, ElektraKeyset * defaults)
{
	elektraKeysetRewind (defaults);
	for (ElektraKey * key = elektraKeysetNext (defaults); key != NULL; key = elektraKeysetNext (defaults))
	{
		ElektraKey * const dup = elektraKeyDup (key, ELEKTRA_KEY_CP_ALL);
		const char * name = elektraKeyName (key);
		elektraKeySetName (dup, elektraKeyName (parentKey));
		elektraKeyAddName (dup, name);

		if (strlen (elektraKeyString (dup)) == 0)
		{
			const ElektraKey * defaultMeta = elektraKeyGetMeta (dup, "default");
			if (defaultMeta != NULL)
			{
				elektraKeySetString (dup, elektraKeyString (defaultMeta));
			}
		}

		elektraKeysetAppendKey (config, dup);
	}
}


#ifdef __cplusplus
};
#endif

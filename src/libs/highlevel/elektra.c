/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <elektra/core/errors.h>
#include <elektra/ease/spec.h>
#include <elektra/highlevel.h>
#include <elektra/highlevel/types.h>
#include <elektra/type/conversion.h>
#include <internal/kdbprivate.h>
#include <internal/macros/symver.h>
#include <internal/utility/logger.h>
#include <internal/utility/array.h>
#include <internal/utility/format.h>
#include <internal/utility/alloc.h>
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

static void insertDefaults (KeySet * config, const Key * parentKey, KeySet * defaults);

static kdb_boolean_t checkSpecProperlyMounted (KDB * const kdb, const char * application, ElektraError ** error);
static kdb_boolean_t checkSpecificationMountPoint (KeySet * const mountPointsKs, const char * application, const char * mountPoint,
						   ElektraError ** error);
static kdb_boolean_t checkSpecToken (KDB * const kdb, Key * parentKey, const char * tokenFromContract, ElektraError ** error);
static char * generateSpecProblemErrorMessage (const char * application);

/**
 * \defgroup highlevel High-level API
 * @{
 */

kdb_boolean_t checkSpec (Key * const parentKey, KeySet * contract, ElektraError ** error);
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
Elektra * elektraOpen (const char * application, KeySet * defaults, KeySet * contract, ElektraError ** error)
{
	Key * const parentKey = keyNew (application, KEY_END);

	// Before anything else: Verify that the specification is okay.
	if (!checkSpec (parentKey, contract, error))
	{
		keyDel (parentKey);
		return NULL;
	}

	KDB * const kdb = kdbOpen (contract, parentKey);

	if (kdb == NULL)
	{
		*error = elektraErrorFromKey (parentKey);
		keyDel (parentKey);
		return NULL;
	}

	int ignoreRequireInHelpMode = 0;

	if (contract != NULL)
	{
		// TODO: set default spec config to use ERROR
		ksAppendKey (contract, keyNew ("system:/elektra/contract/mountglobal/spec", KEY_END));
		ksAppendKey (contract,
			     keyNew ("system:/elektra/contract/mountglobal/spec/config/conflict/get", KEY_VALUE, "ERROR", KEY_END));
		ksAppendKey (contract,
			     keyNew ("system:/elektra/contract/mountglobal/spec/config/conflict/set", KEY_VALUE, "ERROR", KEY_END));
		ksAppendKey (contract, keyNew ("system:/elektra/contract/mountglobal/spec/config/missing/log", KEY_VALUE, "1", KEY_END));

		Key * contractCut = keyNew ("system:/elektra/contract/highlevel", KEY_END);
		KeySet * highlevelContract = ksCut (contract, contractCut);

		if (ksGetSize (highlevelContract) > 0)
		{
			ksAppend (contract, highlevelContract);
			if (ksLookupByName (highlevelContract, "system:/elektra/contract/highlevel/helpmode/ignore/require", 0) != NULL)
			{
				ignoreRequireInHelpMode = 1;
			}
		}

		keyDel (contractCut);
		ksDel (highlevelContract);
	}

	KeySet * const config = ksNew (0, KS_END);
	if (defaults != NULL)
	{
		insertDefaults (config, parentKey, defaults);
	}

	const int kdbGetResult = kdbGet (kdb, config, parentKey);
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
			ksDel (config);
			kdbClose (kdb, parentKey);
			keyDel (parentKey);
			return NULL;
		}
		elektraErrorReset (&errorFromKdbGet);
	}
	// If kdbGet() returns -1, there was an error.
	else if (kdbGetResult == -1)
	{
		Key * helpKey = ksLookupByName (config, "proc:/elektra/gopts/help", 0);
		const Key * missingKeys = keyGetMeta (parentKey, "logs/spec/missing");
		if (ignoreRequireInHelpMode == 1 && helpKey != NULL && missingKeys != NULL)
		{
			// proc:/elektra/gopts/help was set -> we know we are in help mode
			// logs/spec/missing exists on parentKey -> we know that spec detected missing keys
			// we ensured that spec uses conflict/get = ERROR -> we know that the error in kdbGet must be from spec
			// --> Therefore, we are in the error case that should be ignored

			// BUT: anything other than helpKey may be incorrect
			// and only helpKey should be used anyway
			// so create a new config KeySet
			Key * helpKeyDup = keyDup (helpKey, KEY_CP_ALL);
			ksClear (config);
			ksAppendKey (config, helpKeyDup);
		}
		else
		{
			// We are in the error case that should not be ignored.
			*error = elektraErrorFromKey (parentKey);

			ksDel (config);
			kdbClose (kdb, parentKey);
			keyDel (parentKey);
			return NULL;
		}
	}

	Elektra * const elektra = elektraCalloc (sizeof (struct _Elektra));
	elektra->kdb = kdb;
	elektra->parentKey = parentKey;
	elektra->parentKeyLength = keyGetNameSize (parentKey) - 1;
	elektra->config = config;
	elektra->lookupKey = keyNew ("/", KEY_END);
	elektra->fatalErrorHandler = &defaultFatalErrorHandler;
	elektra->defaults = ksDup (defaults);

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
kdb_boolean_t checkSpec (Key * const parentKey, KeySet * contract, ElektraError ** error)
{
	if (contract != NULL)
	{
		// Execute kdbOpen(), but do not pass the contract!
		// Reason: The contract may contain requirements that interfere with the checks below.
		// E.g., If the contract contains keys from elektraGOptsContract(), kdbGet() might fail, because the specification is not
		// mounted or incorrect. Therefore, we first have to check, that the spec is properly mounted and hasn't been modified since
		// compilation.
		KDB * kdb = kdbOpen (NULL, parentKey);

		if (kdb == NULL)
		{
			*error = elektraErrorFromKey (parentKey);
			return false;
		}

		// Verify that the specification is properly mounted, if contract requires it.
		Key * checkSpecFromContract = ksLookupByName (contract, "system:/elektra/contract/highlevel/check/spec/mounted", 0);
		kdb_boolean_t shouldCheckSpecProperlyMounted = false;
		if (checkSpecFromContract != NULL && elektraKeyToBoolean (checkSpecFromContract, &shouldCheckSpecProperlyMounted) &&
		    shouldCheckSpecProperlyMounted)
		{
			// If the specification was not properly mounted, we don't return an Elektra instance.
			// Reason: the application won't function properly without a properly mounted specification.
			if (!checkSpecProperlyMounted (kdb, keyName (parentKey), error))
			{
				kdbClose (kdb, parentKey);
				keyDel (checkSpecFromContract);
				return false;
			}

			// If the contract contains a specification token, verify that is is equal to the current specification token.
			Key * tokenFromContractKey = ksLookupByName (contract, "system:/elektra/contract/highlevel/check/spec/token", 0);
			const char * tokenFromContract = NULL;
			if (tokenFromContractKey != NULL && elektraKeyToString (tokenFromContractKey, &tokenFromContract) &&
			    tokenFromContract != NULL && strlen (tokenFromContract) > 0)
			{
				if (!checkSpecToken (kdb, parentKey, tokenFromContract, error))
				{
					kdbClose (kdb, parentKey);
					keyDel (checkSpecFromContract);
					keyDel (tokenFromContractKey);
					return false;
				}
			}
			keyDel (tokenFromContractKey);
		}
		keyDel (checkSpecFromContract);
		kdbClose (kdb, parentKey);
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
kdb_boolean_t checkSpecToken (KDB * const kdb, Key * parentKey, const char * tokenFromContract, ElektraError ** error)
{
	KeySet * const specificationKs = ksNew (0, KS_END);

	Key * parentKeySpecNamespace = keyDup (parentKey, KEY_CP_ALL);
	// For token calculation of an application using the HL API, only keys within the spec namespace are relevant.
	keySetNamespace (parentKeySpecNamespace, KEY_NS_SPEC);

	const int kdbGetResult = kdbGet (kdb, specificationKs, parentKeySpecNamespace);
	if (kdbGetResult == -1)
	{
		ksDel (specificationKs);
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
			ksDel (specificationKs);
			*error = elektraErrorFromKey (parentKeySpecNamespace);
			keyDel (parentKeySpecNamespace);
			return false;
		}

		// If tokens aren't equal, report an error and fail
		if (strcmp (tokenFromContract, calculatedToken) != 0)
		{
			char * errorMessage = generateSpecProblemErrorMessage (keyName (parentKey));
			char * description = elektraFormat (
				"%s\n"
				"Technical details: The configuration specification on your system was modified after installation.\n"
				"The token was \"%s\" during compilation\nbut now it's \"%s\"\n",
				errorMessage, tokenFromContract, calculatedToken);
			elektraFree (errorMessage);
			*error = elektraErrorCreate (ELEKTRA_ERROR_VALIDATION_SEMANTIC, description, "highlevel", "unknown", 0);
			elektraFree (description);
			ksDel (specificationKs);
			keyDel (parentKeySpecNamespace);
			return false;
		}
	}
	return true;
}

Elektra * ELEKTRA_SYMVER (elektraOpen, v1) (const char * application, KeySet * defaults, ElektraError ** error)
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
static kdb_boolean_t checkSpecProperlyMounted (KDB * const kdb, const char * application, ElektraError ** error)
{
	KeySet * const mountPoints = ksNew (0, KS_END);
	Key * const parentKey = keyNew ("system:/elektra/mountpoints", KEY_END);
	const int kdbGetResult = kdbGet (kdb, mountPoints, parentKey);

	if (kdbGetResult == -1)
	{
		ksDel (mountPoints);
		*error = elektraErrorFromKey (parentKey);
		keyDel (parentKey);
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
		ksDel (mountPoints);
		keyDel (parentKey);
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
static kdb_boolean_t checkSpecificationMountPoint (KeySet * const mountPointsKs, const char * application, const char * mountPoint,
						   ElektraError ** error)
{
	Key * mountPointLookupKey = keyNew ("system:/elektra/mountpoints/", KEY_END);
	keyAddBaseName (mountPointLookupKey, mountPoint);
	keyAddName (mountPointLookupKey, "plugins/backend/name");

	bool mountPointExists = ksLookup (mountPointsKs, mountPointLookupKey, 0) != NULL;
	if (!mountPointExists)
	{
		char * errorMessage = generateSpecProblemErrorMessage (application);
		char * description = elektraFormat (
			"%s\n"
			"Technical details: \n"
			"The key \"%s\" should exist, but it does not.\n"
			"This was likely caused by an incomplete installation of the application.\n",
			errorMessage, keyName (mountPointLookupKey));
		elektraFree (errorMessage);
		*error = elektraErrorCreate (ELEKTRA_ERROR_INSTALLATION, description, "elektra", "unknown", 0);
		elektraFree (description);
	}

	keyDel (mountPointLookupKey);
	return mountPointExists;
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
Key * elektraHelpKey (Elektra * elektra)
{
	return ksLookupByName (elektra->config, "proc:/elektra/gopts/help", 0);
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

KDBType const KDB_TYPE_STRING = "string";
KDBType const KDB_TYPE_BOOLEAN = "boolean";
KDBType const KDB_TYPE_CHAR = "char";
KDBType const KDB_TYPE_OCTET = "octet";
KDBType const KDB_TYPE_SHORT = "short";
KDBType const KDB_TYPE_UNSIGNED_SHORT = "unsigned_short";
KDBType const KDB_TYPE_LONG = "long";
KDBType const KDB_TYPE_UNSIGNED_LONG = "unsigned_long";
KDBType const KDB_TYPE_LONG_LONG = "long_long";
KDBType const KDB_TYPE_UNSIGNED_LONG_LONG = "unsigned_long_long";
KDBType const KDB_TYPE_FLOAT = "float";
KDBType const KDB_TYPE_LONG_DOUBLE = "long_double";
KDBType const KDB_TYPE_DOUBLE = "double";
KDBType const KDB_TYPE_ENUM = "enum";

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

			/* TODO: Remove call the ksCurrent () because the internal iterators are deprecated
			 * PROBLEM: Change in kdbSet necessary (additional output parameter for position?, change return value?) */
			Key * problemKey = ksCurrent (elektra->config);
			if (problemKey != NULL)
			{
				ELEKTRA_LOG_DEBUG ("problemKey: %s\n", keyName (problemKey));
			}

			key = keyDup (key, KEY_CP_ALL);
			kdbGet (elektra->kdb, elektra->config, elektra->parentKey);
		}
	} while (ret == -1);
}

void insertDefaults (KeySet * config, const Key * parentKey, KeySet * defaults)
{
	for (elektraCursor it = 0; it < ksGetSize (defaults); ++it)
	{
		Key * key = ksAtCursor (defaults, it);
		Key * const dup = keyDup (key, KEY_CP_ALL);
		const char * name = keyName (key);
		keySetName (dup, keyName (parentKey));
		keyAddName (dup, name);

		if (strlen (keyString (dup)) == 0)
		{
			const Key * defaultMeta = keyGetMeta (dup, "default");
			if (defaultMeta != NULL)
			{
				keySetString (dup, keyString (defaultMeta));
			}
		}

		ksAppendKey (config, dup);
	}
}


#ifdef __cplusplus
};
#endif

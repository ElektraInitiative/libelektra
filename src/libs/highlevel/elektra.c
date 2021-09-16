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

static void insertDefaults (KeySet * config, const Key * parentKey, KeySet * defaults);
static bool checkHighlevelContract (const char * application, KeySet * contract, ElektraError ** error);

static kdb_boolean_t checkSpecProperlyMounted (KDB * const kdb, const char * application, ElektraError ** error);
static kdb_boolean_t checkSpecificationMountPoint (KeySet * const mountPointsKs, const char * application, const char * mountPoint,
						   ElektraError ** error);

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

			if (!checkHighlevelContract (application, highlevelContract, error))
			{
				keyDel (contractCut);
				ksDel (highlevelContract);

				kdbClose (kdb, parentKey);
				keyDel (parentKey);

				return NULL;
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

	if (kdbGetResult == -1)
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

	char hash_string[65];
	Key * parentKeySpecNamespace = keyDup(parentKey, KEY_CP_ALL);
	// For token calculation of an application using the HL API, only keys within the spec namespace are relevant.
	keySetNamespace(parentKeySpecNamespace, KEY_NS_SPEC);
	calculateSpecificationToken(hash_string, config, parentKeySpecNamespace);
	keyDel(parentKeySpecNamespace);

	// If the specification was not properly mounted, we don't return an Elektra instance.
	// Reason: the application won't function properly without a properly mounted specification.
	if (!checkSpecProperlyMounted (kdb, application, error))
	{
		ksDel (config);
		kdbClose (kdb, parentKey);
		keyDel (parentKey);
		return NULL;
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
	// Construct the lookup key
	Key * mountPointLookupKey = keyNew ("system:/elektra/mountpoints/", KEY_END);
	keyAddBaseName (mountPointLookupKey, mountPoint);
	keyAddBaseName (mountPointLookupKey, "mountpoint");

	Key * mountPointKey = ksLookup (mountPointsKs, mountPointLookupKey, 0);
	// If the mountPointKey does not exist, the specification was not properly mounted.
	if (mountPointKey == NULL)
	{
		char * description = elektraFormat (
			"The specification for application '%s' was not properly mounted. This is likely caused by an incomplete "
			"installation of the application. Please consult the application's documentation or contact its developers. "
			"Technical detail: The mountPointKey '%s' should exist, but it does not.",
			application, keyName (mountPointLookupKey));
		keyDel (mountPointLookupKey);
		*error = elektraErrorCreate (ELEKTRA_ERROR_INSTALLATION, description, "elektra", "unknown", 0);
		elektraFree (description);
		return false;
	}
	// If the mountPointKey's value is not equal to "application", the specification was not properly mounted.
	else if (elektraStrCmp (keyString (mountPointKey), mountPoint) != 0)
	{
		char * description = elektraFormat (
			"The specification for application %s was not properly mounted. This is likely caused by an incomplete "
			"installation of the application. Please consult the application's documentation or contact its developers. "
			"Technical detail: The value of mountPointKey '%s' should match the application name '%s', but it does not.",
			application, keyName (mountPointLookupKey), application);
		*error = elektraErrorCreate (ELEKTRA_ERROR_INSTALLATION, description, "elektra", "unknown", 0);
		keyDel (mountPointLookupKey);
		keyDel (mountPointKey);
		elektraFree (description);
		return false;
	}
	// Both checks succeeded.
	else
	{
		keyDel (mountPointLookupKey);
		keyDel (mountPointKey);
		return true;
	}
}

/**
 * Check whether the specification file for @p application was properly spec-mounted using "kdb spec-mount"
 *
 * Note: @kodebach pointed out: Technically this is not a 100% correct check, but it works most of the time (namely when kdb mount/kdb
 * spec-mount was used). The layout of system:/elektra/mountpoints will change with #3693, so "works most of the time" is fine for now. When
 * the backend implementation is done, we can create a better solution.
 * @param kdb		The KDB instance used for checking.
 * @param application 	The application's base name.
 * @param error		Pointer used to report errors.
 * @return 		True if the specification file was properly spec-mounted, false otherwise.
 */
/*kdb_boolean_t specMountExecuted (KDB * const kdb, const char * application, ElektraError ** error)
{
	char * description = elektraFormat ("'kdb spec-mount' was not properly executed for application %s. This is likely caused by an
incomplete installation of the application. Please consult the application's documentation or contact its developers.", application);
	*error = elektraErrorCreate (ELEKTRA_ERROR_INSTALLATION, description, "elektra", "unknown", 0);
	return 0;
}*/

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

			key = keyDup (key, KEY_CP_ALL);
			kdbGet (elektra->kdb, elektra->config, elektra->parentKey);
		}
	} while (ret == -1);
}

void insertDefaults (KeySet * config, const Key * parentKey, KeySet * defaults)
{
	ksRewind (defaults);
	for (Key * key = ksNext (defaults); key != NULL; key = ksNext (defaults))
	{
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

static bool minimalValidation (const char * application)
{
	Key * parent = keyNew ("system:/elektra/mountpoints", KEY_END);
	KDB * kdb = kdbOpen (NULL, parent);
	KeySet * mountpoints = ksNew (0, KS_END);
	if (kdbGet (kdb, mountpoints, parent) < 0)
	{
		ksDel (mountpoints);
		kdbClose (kdb, parent);
		keyDel (parent);
		return false;
	}

	char * specName = elektraFormat ("spec%s", application);
	Key * lookup = keyNew ("system:/elektra/mountpoints", KEY_END);
	keyAddBaseName (lookup, specName);
	elektraFree (specName);

	if (ksLookup (mountpoints, lookup, 0) == NULL)
	{
		keyDel (lookup);

		ksDel (mountpoints);
		kdbClose (kdb, parent);
		keyDel (parent);
		return false;
	}

	keyDel (lookup);

	lookup = keyNew ("system:/elektra/mountpoints", KEY_END);
	keyAddBaseName (lookup, application);

	if (ksLookup (mountpoints, lookup, 0) == NULL)
	{
		keyDel (lookup);

		ksDel (mountpoints);
		kdbClose (kdb, parent);
		keyDel (parent);
		return false;
	}
	keyDel (lookup);

	ksDel (mountpoints);
	kdbClose (kdb, parent);
	keyDel (parent);

	return true;
}

bool checkHighlevelContract (const char * application, KeySet * contract, ElektraError ** error)
{
	Key * validationKey = ksLookupByName (contract, "system:/elektra/contract/highlevel/validation", 0);
	if (validationKey != NULL)
	{
		if (strcmp (keyString (validationKey), "minimal") == 0 && !minimalValidation (application))
		{
			*error = elektraErrorMinimalValidationFailed (application);
			return false;
		}
	}

	return true;
}


#ifdef __cplusplus
};
#endif

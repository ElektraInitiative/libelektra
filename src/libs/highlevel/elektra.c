/**
 * @file
 *
 * @brief Elektra High Level API.
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "elektra.h"
#include "elektra_conversion.h"
#include "elektra_error_private.h"
#include "elektra_private.h"
#include "elektra_types.h"
#include "kdblogger.h"

static void defaultFatalErrorHandler (ElektraError * error)
{
	ELEKTRA_LOG_DEBUG ("FATAL ERROR [%d]: %s", error->code, error->description);
	ElektraErrorCode code = error->code;
	elektraFree (error);
	exit (code);
}

/**
 * \defgroup highlevel High-level API
 * @{
 */

ELEKTRA_TAG_DEFINITIONS (const char *, String, KDB_TYPE_STRING, elektraStrDup, elektraKeyToString)
ELEKTRA_TAG_DEFINITIONS (kdb_boolean_t, Boolean, KDB_TYPE_BOOLEAN, elektraBooleanToString, elektraKeyToBoolean)
ELEKTRA_TAG_DEFINITIONS (kdb_char_t, Char, KDB_TYPE_CHAR, elektraCharToString, elektraKeyToChar)
ELEKTRA_TAG_DEFINITIONS (kdb_octet_t, Octet, KDB_TYPE_OCTET, elektraOctetToString, elektraKeyToOctet)
ELEKTRA_TAG_DEFINITIONS (kdb_short_t, Short, KDB_TYPE_SHORT, elektraShortToString, elektraKeyToShort)
ELEKTRA_TAG_DEFINITIONS (kdb_unsigned_short_t, UnsignedShort, KDB_TYPE_UNSIGNED_SHORT, elektraUnsignedShortToString,
			 elektraKeyToUnsignedShort)
ELEKTRA_TAG_DEFINITIONS (kdb_long_t, Long, KDB_TYPE_LONG, elektraLongToString, elektraKeyToLong)
ELEKTRA_TAG_DEFINITIONS (kdb_unsigned_long_t, UnsignedLong, KDB_TYPE_UNSIGNED_LONG, elektraUnsignedLongToString, elektraKeyToUnsignedLong)
ELEKTRA_TAG_DEFINITIONS (kdb_long_long_t, LongLong, KDB_TYPE_LONG_LONG, elektraLongLongToString, elektraKeyToLongLong)
ELEKTRA_TAG_DEFINITIONS (kdb_unsigned_long_long_t, UnsignedLongLong, KDB_TYPE_UNSIGNED_LONG_LONG, elektraLongLongToString,
			 elektraKeyToUnsignedLongLong)
ELEKTRA_TAG_DEFINITIONS (kdb_float_t, Float, KDB_TYPE_FLOAT, elektraFloatToString, elektraKeyToFloat)
ELEKTRA_TAG_DEFINITIONS (kdb_double_t, Double, KDB_TYPE_DOUBLE, elektraDoubleToString, elektraKeyToDouble)

#if defined(HAVE_SIZEOF_LONG_DOUBLE) && (SIZEOF_LONG_DOUBLE == 16 || SIZEOF_LONG_DOUBLE == 12)

ELEKTRA_TAG_DEFINITIONS (kdb_long_double_t, LongDouble, KDB_TYPE_LONG_DOUBLE, elektraLongDoubleToString, elektraKeyToLongDouble)

#endif // HAVE_SIZEOF_LONG_DOUBLE

ELEKTRA_TAG_DEFINITIONS (int, Enum, KDB_TYPE_ENUM, elektraLongToString, elektraKeyToLong)

/**
 * Initializes a new Elektra instance.
 * @param application The parent key for your application.
 * @param defaults A KeySet containing default values. Passing NULL means "no default values".
 * @return An Elektra instance initialized with the application.
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
	elektra->enforceType = true;
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
 * Sets whether type metadata should be enforced by this Elektra instance.
 *
 * Even if type metadata is not enforce, setting a value will always set the
 * corresponding type metadata value too.
 *
 * @param elektra             An Elektra instance.
 * @param enforceTypeMetadata Set to #true to enforce type metadata.
 */
void elektraEnforceTypeMetadata (Elektra * elektra, bool enforceTypeMetadata)
{
	elektra->enforceType = enforceTypeMetadata;
}

/**
 * Releases all ressources used by the given elektra instance. The elektra instance must not be used anymore after calling this.
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

void elektraSetArrayLookupKey (Elektra * elektra, const char * name, size_t index)
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
			if (elektraErrorCode (kdbSetError) != 30) // ELEKTRA_ERROR_CONFLICT = 30
			{
				*error = kdbSetError;
				return;
			}

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

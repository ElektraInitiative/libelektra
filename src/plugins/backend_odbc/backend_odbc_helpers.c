/**
 * @file
 *
 * @brief Helper functions for working with ODBC data sources
 *
 * This file contains constants, structs and functions that are used by the ODBC backend plugin.
 * They are generally usefully when working with ODBC data sources.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "backend_odbc_helpers.h"

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>

#include <sqlext.h>
#include <stdio.h> /* for sprintf */
#include <stdlib.h>
#include <string.h>

/**
 * @internal
 *
 * @brief Get the number of digits an integer value consists of (e.g. 123 has 3 digits, therefore 3 is returned)
 *
 * @param i The integer value from which the number of digits should be computed
 *
 * @return The number of digits the given integer value consists of
 */
static unsigned char getNumDigits (int i)
{
	unsigned char digits = 1;

	while ((i /= 10) != 0)
	{
		digits++;
	}

	return digits;
}


/**
 * @brief Get a string array with all currently present errors for the given ODBC handle
 *
 * @param handleType The type of the handle (environment, connection, statement, descriptor)
 * 	environment:	SQL_HANDLE_ENV
 * 	connection:	SQL_HANDLE_DBC
 * 	statement:	SQL_HANDLE_STMT
 * 	descriptor;	SQL_HANDLE_DESC
 *
 * @param odbcHandle The actual valid handle which must match the type given in @p handleType
 *
 * @return An array of strings with the current error messages for the given handle
 * 	The last element of the array is set to NULL.
 *	Make sure to free the returned strings as well as the string array itself.
 * @retval NULL if no error or warning messages are present
 */
char ** extractOdbcErrors (SQLSMALLINT handleType, SQLHANDLE odbcHandle)
{
	SQLINTEGER nativeErrCode;
	SQLCHAR state[SQL_SQLSTATE_SIZE + 1];
	SQLCHAR * text = NULL;
	SQLSMALLINT lenText;
	SQLSMALLINT maxLenText = 0;
	char ** resultArray = NULL;
	int msgCount;

	SQLINTEGER recNum = 1;
	/* Detect maximum text length */
	for (msgCount = 0; SQL_SUCCEEDED (SQLGetDiagRec (handleType, odbcHandle, recNum++, state, &nativeErrCode, text, 0, &lenText));
	     msgCount++)
	{
		if (lenText > maxLenText)
		{
			maxLenText = lenText;
		}
	}

	ELEKTRA_ASSERT (msgCount >= 0,
			"The message counter reached a value which indicates a negative count of messages, this looks like "
			"a bug! Please report this bug at https://issues.libelektra.org.");

	if (msgCount == 0)
	{
		return NULL;
	}

	text = elektraMalloc (maxLenText + 1);
	resultArray = elektraMalloc ((msgCount + 1) * sizeof (char *));

	recNum = 1;
	for (msgCount = 0;
	     SQL_SUCCEEDED (SQLGetDiagRec (handleType, odbcHandle, recNum++, state, &nativeErrCode, text, maxLenText + 1, &lenText));
	     msgCount++)
	{
		/* 5 extra chars (3x ':', 1x '1', 1x '\0') */
		resultArray[msgCount] = elektraMalloc (SQL_SQLSTATE_SIZE + maxLenText + getNumDigits (nativeErrCode) + 5);
		sprintf (resultArray[msgCount], "%s:%d:%d:%s", state, 1, nativeErrCode, text);
	}

	elektraFree (text);
	return resultArray;
}


/**
 * @brief Set an ODBC error or add ODBC warnings to @p errorKey
 *
 * @param handleType The type of the handle (environment, connection, statement, descriptor)
 * 	environment:	SQL_HANDLE_ENV
 * 	connection:	SQL_HANDLE_DBC
 * 	statement:	SQL_HANDLE_STMT
 * 	descriptor:	SQL_HANDLE_DESC
 * @param handle The actual valid ODBC handle which must match the type given in @p handleType
 * @param functionName The name of the function in which the error or warning(s) were emitted
 * @param isWarning true: treat first record as warning, false: treat first record as error
 * @param errorKey Used to store errors and warnings
 *
 * @return The number of processed errors and warnings
 * @retval 0 if no error or warning was found
 * @retval -1 if an error occurred in this function (e.g. invalid handle given)
 */
int setOdbcError (SQLSMALLINT handleType, SQLHANDLE handle, const char * fileName, const char * functionName, const char * lineNo,
		  bool isWarning, Key * errorKey)
{
	/* Get number of available status records */
	SQLSMALLINT numRecs = 0;

	if (!SQL_SUCCEEDED (SQLGetDiagField (handleType, handle, 0, SQL_DIAG_NUMBER, &numRecs, 0, 0)))
	{
		return -1;
	}

	/* Get the status records */
	SQLSMALLINT i;
	for (i = 1; i <= numRecs; i++)
	{
		SQLCHAR sqlState[SQL_SQLSTATE_SIZE + 1];
		SQLCHAR errMsg[SQL_MAX_MESSAGE_LENGTH + 1];
		SQLINTEGER nativeError;
		SQLSMALLINT errMsgLen;


		SQLRETURN ret =
			SQLGetDiagRec (handleType, handle, i, sqlState, &nativeError, errMsg, SQL_MAX_MESSAGE_LENGTH + 1, &errMsgLen);

		if (ret == SQL_NO_DATA)
		{
			/* No additional error or warning present */
			if (i >= 1)
			{
				return i - 1;
			}
			else
			{
				return -1;
			}
		}
		else if (!SQL_SUCCEEDED (ret))
		{
			return -1;
		}

		/* The official documentation from Microsoft states that "There is no maximum length of the diagnostic message text".
		 * Therefore, we check the actual length despite using the SQL_MAX_MESSAGE_LENGTH constant.
		 * see: https://learn.microsoft.com/en-us/sql/odbc/reference/syntax/sqlgetdiagrec-function (Arguments->BufferLength) */
		SQLCHAR * longErrMsg;

		if (ret == SQL_SUCCESS_WITH_INFO && errMsgLen > SQL_MAX_MESSAGE_LENGTH)
		{
			longErrMsg = (SQLCHAR *) elektraMalloc (sizeof (SQLCHAR) * (errMsgLen + 1));

			/* Use the truncated error message in 'errMsg' if the allocation has failed */
			if (longErrMsg)
			{
				SQLGetDiagRec (handleType, handle, i, sqlState, &nativeError, longErrMsg, errMsgLen, 0);
			}
		}
		else
		{
			longErrMsg = NULL;
		}

		/* Only one error can be set, all other ODBC errors are added as  (already done by the Elektra error/warning API)
		 * According to the definition, records that describe errors come first
		 * see: https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/sequence-of-status-records */
		if (!isWarning)
		{
			elektraSetErrorINTERNAL (errorKey, fileName, lineNo, ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),
						 "An ODBC function returned an error for the mountpoint '%s'\n"
						 "in the function '%s'\n"
						 "The following information is from the ODBC library:\n"
						 "Number of error: %d of %d\n"
						 "Status code: %s\n"
						 "Native error code: %d\n"
						 "Error message: %s\n",
						 keyString (errorKey), functionName ? functionName : "", i, numRecs, sqlState, nativeError,
						 longErrMsg ? longErrMsg : errMsg);
		}
		else
		{
			elektraAddWarningINTERNAL (errorKey, fileName, lineNo, ELEKTRA_STRINGIFY (ELEKTRA_MODULE_NAME),
						   "An ODBC function returned an error for the mountpoint '%s'\n"
						   "in the function '%s'\n"
						   "The following information is from the ODBC library:\n"
						   "Number of error: %d of %d\n"
						   "Status code: %s\n"
						   "Native error code: %d\n"
						   "Error message: %s\n",
						   keyString (errorKey), functionName ? functionName : "", i, numRecs, sqlState,
						   nativeError, longErrMsg ? longErrMsg : errMsg);
		}
		elektraFree (longErrMsg);
	}

	if (numRecs > 0 && i > numRecs)
	{
		return numRecs;
	}
	else if (numRecs == 0)
	{
		return 0;
	}
	else
	{
		return -1;
	}
}

/**
 * @brief Get the name of all data sources that were defined in the ODBC configuration
 *
 * @return An array of strings with the ODBC data source names
 * 	The last entry is set to NULL to indicate the end of the array.
 * 	Make sure to free the returned strings as well as the string array itself.
 * @retval NULL if no data sources were defined or an error occurred while fetching the data source names
 */
char ** getAvailableDataSources (void)
{
	SQLHENV env;

	char * dsn = NULL;
	SQLSMALLINT lenDsn;
	SQLSMALLINT maxLenDsn = 0;

	int dsnCount = 0;
	char ** result = NULL;

	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env);
	SQLSetEnvAttr (env, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) SQL_OV_ODBC3, 0);


	for (SQLUSMALLINT direction = SQL_FETCH_FIRST;
	     SQL_SUCCEEDED (SQLDataSources (env, direction, (SQLCHAR *) dsn, 0, &lenDsn, NULL, 0, NULL)); direction = SQL_FETCH_NEXT)
	{
		if (lenDsn > maxLenDsn)
		{
			maxLenDsn = lenDsn;
		}

		dsnCount++;
	}


	ELEKTRA_ASSERT (dsnCount >= 0,
			"The datasource counter reached a value which indicates a negative count of data sources, this "
			"looks like a bug! Please report this bug at https://issues.libelektra.org.");
	if (dsnCount == 0)
	{
		/* No data sources found */
		return NULL;
	}

	/* We need one entry for the NULL indicating the end of the returned string array */
	result = elektraCalloc ((dsnCount + 1) * sizeof (char *));

	/* Add one byte for \0 */
	dsn = elektraMalloc ((maxLenDsn + 1) * sizeof (char));


	dsnCount = 0;

	for (SQLUSMALLINT direction = SQL_FETCH_FIRST;
	     SQL_SUCCEEDED (SQLDataSources (env, direction, (SQLCHAR *) dsn, maxLenDsn + 1, &lenDsn, NULL, 0, NULL));
	     direction = SQL_FETCH_NEXT)
	{
		result[dsnCount] = elektraCalloc ((lenDsn + 1) * sizeof (char));
		strcpy (result[dsnCount], (char *) dsn);
		dsnCount++;
	}

	SQLFreeHandle (SQL_HANDLE_ENV, env);
	return result;
}


/**
 * @internal
 *
 * @brief Lookup the string value of a Key in a KeySet
 *
 * @param ks The KeySet which should be searched for the Key
 * @param keyName The name of the Key from which that string-value should be copied
 *
 * @return The string value of the looked up key, copied to newly allocated memory
 * 	Make sure to free the returned string.

 * @retval NULL if the Key could not be found, didn't contain a string-value or memory allocation for the string to copy failed
 */
static char * lookupStringFromKs (KeySet * ks, const char * keyName)
{
	/* Returns NULL if 'ks' or 'keyName' are NULL or no key was found */
	Key * resultKey = ksLookupByName (ks, keyName, KDB_O_NONE);

	if (!resultKey)
	{
		return NULL;
	}


	return elektraStrDup (keyString (resultKey));
}


/**
 * @brief Creates a struct for the data source configuration that is provided in a KeySet
 *
 * This function should be used during the initialization of the ODBC backend.
 *
 * @param ksDefinition The KeySet with the mountpoint definition as provided to the init function of a backend plugin
 *
 * @return The filled struct with the configuration of the datasource
 * 	Make sure to free the struct itself as well as the strings it contains.
 *
 * @retval NULL if a mandatory configuration value was not present in the given KeySet, a NULL pointer was given, or memory allocation
 * failed
 *
 * @see ELEKTRA_PLUGIN_FUNCTION (init)
 * @see https://www.libelektra.org/devdocu/backend-plugins
 */
struct dataSourceConfig * fillDsStructFromDefinitionKs (KeySet * ksDefinition, Key * errorKey)
{
	if (!ksDefinition)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "Got NULL for the 'ksDefinition' argument.");
		return NULL;
	}

	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	if (!dsConfig)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		return NULL;
	}

	/* Get configuration data for the ODBC data source */

	dsConfig->dataSourceName = lookupStringFromKs (ksDefinition, "system:/dataSourceName");

	if (!(dsConfig->dataSourceName))
	{
		ELEKTRA_SET_INTERFACE_ERROR (
			errorKey, "The mandatory value 'dataSourceName' was missing from the mountpoint definition in 'ksDefinition'.");
		goto errFillDsFromConf;
	}


	/* Username and password are optional (NULL = no username/password)
	 * Some data sources don't require credentials or have them configured as part of the ODBC data source definition
	 * (e.g. in odbc.ini)
	 */
	dsConfig->userName = lookupStringFromKs (ksDefinition, "system:/userName");
	dsConfig->password = lookupStringFromKs (ksDefinition, "system:/password");

	dsConfig->tableName = lookupStringFromKs (ksDefinition, "system:/table/name");
	if (!(dsConfig->tableName))
	{
		ELEKTRA_SET_INTERFACE_ERROR (
			errorKey, "The mandatory value 'tableName' was missing from the mountpoint definition in 'ksDefinition'.");
		goto errFillDsFromConf;
	}


	dsConfig->keyColName = lookupStringFromKs (ksDefinition, "system:/table/keyColName");
	if (!(dsConfig->keyColName))
	{
		ELEKTRA_SET_INTERFACE_ERROR (
			errorKey, "The mandatory value 'keyColName' was missing from the mountpoint definition in 'ksDefinition'.");
		goto errFillDsFromConf;
	}

	dsConfig->valColName = lookupStringFromKs (ksDefinition, "system:/table/valColName");

	if (!(dsConfig->valColName))
	{
		ELEKTRA_SET_INTERFACE_ERROR (
			errorKey, "The mandatory value 'valColName' was missing from the mountpoint definition in 'ksDefinition'.");
		goto errFillDsFromConf;
	}


	dsConfig->metaTableName = lookupStringFromKs (ksDefinition, "system:/metaTable/name");

	/* TODO: also support data sources without metatables (then no metadata is supported for such mountpoints)
	 * This can be useful for ODBC drivers which don't support outer joins. */
	if (!(dsConfig->metaTableName))
	{
		ELEKTRA_SET_INTERFACE_ERROR (
			errorKey, "The mandatory value 'metaTableName' was missing from the mountpoint definition in 'ksDefinition'.");
		goto errFillDsFromConf;
	}

	if (dsConfig->metaTableName)
	{
		dsConfig->metaTableKeyColName = lookupStringFromKs (ksDefinition, "system:/metaTable/keyColName");
		if (!(dsConfig->metaTableKeyColName))
		{
			ELEKTRA_SET_INTERFACE_ERROR (errorKey,
						     "The mandatory value 'metaTableKeyColName' was missing from the mountpoint "
						     "definition in 'ksDefinition'.");
			goto errFillDsFromConf;
		}

		dsConfig->metaTableMetaKeyColName = lookupStringFromKs (ksDefinition, "system:/metaTable/metaKeyColName");
		if (!(dsConfig->metaTableMetaKeyColName))
		{
			ELEKTRA_SET_INTERFACE_ERROR (errorKey,
						     "The mandatory value 'metaTableMetaKeyColName' was missing from the "
						     "mountpoint definition in 'ksDefinition'.");
			goto errFillDsFromConf;
		}

		dsConfig->metaTableMetaValColName = lookupStringFromKs (ksDefinition, "system:/metaTable/metaValColName");
		if (!(dsConfig->metaTableMetaValColName))
		{
			ELEKTRA_SET_INTERFACE_ERROR (errorKey,
						     "The mandatory value 'metaTableMetaValColName' was missing from the "
						     "mountpoint definition in 'ksDefinition'.");
			goto errFillDsFromConf;
		}
	}


	/* set default value for timeout to 5 seconds */
	dsConfig->timeOut = 5;

	/* timeout is optional, if no timeout is specified, the default value is used */
	Key * keyTimeout = ksLookupByName (ksDefinition, "system:/timeout", KDB_O_NONE);
	const char * valTimeout = keyString (keyTimeout);
	if (keyTimeout && valTimeout && *valTimeout)
	{
		{
			char * eptr;
			long lTimeout = strtol (valTimeout, &eptr, 10);

			if (*eptr || lTimeout < 0)
			{
				ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (errorKey,
									  "The timeout of the ODBC connection must be "
									  "a non-negative number, but you provided %s.\nTherefore, "
									  "the standard timeout of %hhu seconds is used.",
									  valTimeout, dsConfig->timeOut);
			}
			else if (lTimeout > UCHAR_MAX)
			{
				ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (errorKey,
									  "The maximum value for the timeout of the ODBC "
									  "connection is %d seconds, but you provided %lu.\n"
									  "Therefore, the maximum value of %d is used for the "
									  "timeout.",
									  UCHAR_MAX, lTimeout, UCHAR_MAX);
				dsConfig->timeOut = UCHAR_MAX;
			}
			else
			{
				dsConfig->timeOut = lTimeout;
				ELEKTRA_LOG_DEBUG ("A timeout of %ud seconds was specified for the ODBC connection!", dsConfig->timeOut);
			}
		}
	}
	else
	{
		ELEKTRA_LOG ("No timeout specified for the ODBC connection, using standard timeout of %ud seconds.", dsConfig->timeOut);
	}


	if (dsConfig->timeOut == 0)
	{
		ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNING (errorKey,
							 "You specified '0' for the timeout of the ODBC connection. "
							 "This means that no timeout is used at all and can lead to freezing "
							 "of the application when trying the access an ODBC data source."
							 "This setting is mainly intended for debugging purpose and should "
							 "be used with care.");
	}


	return dsConfig;


errFillDsFromConf:
	/* Free all strings in the struct (NULL is ignored by free()) */
	elektraFree (dsConfig->dataSourceName);
	elektraFree (dsConfig->userName);
	elektraFree (dsConfig->password);
	elektraFree (dsConfig->tableName);
	elektraFree (dsConfig->keyColName);
	elektraFree (dsConfig->valColName);
	elektraFree (dsConfig->metaTableName);
	elektraFree (dsConfig->metaTableKeyColName);
	elektraFree (dsConfig->metaTableMetaKeyColName);
	elektraFree (dsConfig->metaTableMetaValColName);

	/* Free struct */
	elektraFree (dsConfig);
	return NULL;
}

/**
 * @brief Creates a string the contains all mandatory configuration values for an ODBC data source
 *
 * The created string is intended for use as an identifier for a data source.
 * This function should be used during the RESOLVER phase of the ODBC backend.
 *
 * @param dsConfig A valid data source config, as returned by the fillDsStructFromDefinitionKs() function
 *
 * @return A string that contains all fields of the config that identify a data source for the ODBC backend, copied to newly allocated
 * memory Make sure to free the returned string.
 *
 * @retval NULL if an empty configuration struct was passed, no data source name was found or memory allocation failed
 *
 * @see fillDsStructFromDefinitionKs(KeySet * ksDefinition) to get a dataSourceConfig struct from a KeySet with the mountpoint definition
 */
char * dsConfigToString (const struct dataSourceConfig * dsConfig)
{
	if (!dsConfig || !dsConfig->dataSourceName || !(*(dsConfig->dataSourceName)))
	{
		return NULL;
	}

	/* add one char for the \0 and 3 char for separating the values with ' - ' */
	size_t dsConfigStrLen = 1 + (dsConfig->dataSourceName ? strlen (dsConfig->dataSourceName) : 0) +
				(dsConfig->tableName ? 3 + strlen (dsConfig->tableName) : 0) +
				(dsConfig->keyColName ? 3 + strlen (dsConfig->keyColName) : 0) +
				(dsConfig->valColName ? 3 + strlen (dsConfig->valColName) : 0) +
				(dsConfig->metaTableName ? 3 + strlen (dsConfig->metaTableName) : 0) +
				(dsConfig->metaTableKeyColName ? 3 + strlen (dsConfig->metaTableKeyColName) : 0) +
				(dsConfig->metaTableMetaKeyColName ? 3 + strlen (dsConfig->metaTableMetaKeyColName) : 0) +
				(dsConfig->metaTableMetaValColName ? 3 + strlen (dsConfig->metaTableMetaValColName) : 0);

	ELEKTRA_ASSERT (dsConfigStrLen > 1,
			"Calculated length for dsConfig was <= 1. This looks like a bug. Please report this issue at "
			"https://issues.libelektra.org");

	char * retStr = elektraMalloc (dsConfigStrLen * sizeof (char));

	if (!retStr)
	{
		return NULL;
	}

	char * curPart = stpcpy (retStr, dsConfig->dataSourceName);

	if (dsConfig->tableName)
	{
		curPart = stpcpy (curPart, " - ");
		curPart = stpcpy (curPart, dsConfig->tableName);
	}
	if (dsConfig->keyColName)
	{
		curPart = stpcpy (curPart, " - ");
		curPart = stpcpy (curPart, dsConfig->keyColName);
	}
	if (dsConfig->valColName)
	{
		curPart = stpcpy (curPart, " - ");
		curPart = stpcpy (curPart, dsConfig->valColName);
	}
	if (dsConfig->metaTableName)
	{
		curPart = stpcpy (curPart, " - ");
		curPart = stpcpy (curPart, dsConfig->metaTableName);
	}
	if (dsConfig->metaTableKeyColName)
	{
		curPart = stpcpy (curPart, " - ");
		curPart = stpcpy (curPart, dsConfig->metaTableKeyColName);
	}
	if (dsConfig->metaTableMetaKeyColName)
	{
		curPart = stpcpy (curPart, " - ");
		curPart = stpcpy (curPart, dsConfig->metaTableMetaKeyColName);
	}
	if (dsConfig->metaTableMetaValColName)
	{
		curPart = stpcpy (curPart, " - ");
		stpcpy (curPart, dsConfig->metaTableMetaValColName);
	}

	return retStr;
}


/**
 * @brief Check if any identifier that is part of @p dsConfig, does contain the specified substring.
 *
 * @param dsConfig The data source configuration struct to check.
 * @param subStr The substring to check for.
 * @param[out] errorKey If not NULL and if @p subStr was found, an error is set which mentions the name.
 * 	of the first wrong identifier and its value. This parameter is esp. intended for detecting invalid substrings.
 *
 *
 * @retval true if the @p subStr was found in any of the identifiers.
 * @retval false if the @p subStr was NOT found in any of the identifiers.
 * @retval true if @p subStr is null or empty.
 */
bool checkIdentifiersForSubString (const struct dataSourceConfig * dsConfig, const char * subStr, Key * errorKey)
{
	if (!subStr || !(*subStr))
	{
		return true;
	}

	const char * foundIdentifierName = NULL;
	const char * foundIdentifierVal;

	if (strstr (dsConfig->tableName, subStr))
	{
		foundIdentifierName = "tableName";
		foundIdentifierVal = dsConfig->tableName;
	}
	else if (strstr (dsConfig->keyColName, subStr))
	{
		foundIdentifierName = "keyColName";
		foundIdentifierVal = dsConfig->keyColName;
	}
	else if (strstr (dsConfig->valColName, subStr))
	{
		foundIdentifierName = "valColName";
		foundIdentifierVal = dsConfig->valColName;
	}

	else if (strstr (dsConfig->metaTableName, subStr))
	{
		foundIdentifierName = "metaTableName";
		foundIdentifierVal = dsConfig->metaTableName;
	}

	else if (strstr (dsConfig->metaTableKeyColName, subStr))
	{
		foundIdentifierName = "metaTableKeyColName";
		foundIdentifierVal = dsConfig->metaTableKeyColName;
	}

	else if (strstr (dsConfig->metaTableMetaKeyColName, subStr))
	{
		foundIdentifierName = "metaTableMetaKeyColName";
		foundIdentifierVal = dsConfig->metaTableMetaKeyColName;
	}

	else if (strstr (dsConfig->metaTableMetaValColName, subStr))
	{
		foundIdentifierName = "metaTableMetaValColName";
		foundIdentifierVal = dsConfig->metaTableMetaValColName;
	}

	if (foundIdentifierName)
	{
		ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (errorKey,
							 "The identifier %s in the data source configuration contained the "
							 "invalid substring '%s'. The value of the invalid string is '%s'.",
							 foundIdentifierName, subStr, foundIdentifierVal);
		return true;
	}
	else
	{
		/* substr not found */
		return false;
	}
}

/**
 * @brief Free the struct @p dsConfig and (if enabled) the strings it contains
 *
 * @param dsConfig The dataSourceConfig struct which should be freed.
 * @param freeStrings if true, the individual strings (char *) in the struct are freed too.
 */
void clearDsConfig (struct dataSourceConfig * dsConfig, bool freeStrings)
{
	if (!dsConfig)
	{
		return;
	}

	if (freeStrings)
	{
		elektraFree (dsConfig->dataSourceName);

		elektraFree (dsConfig->userName);
		elektraFree (dsConfig->password);

		elektraFree (dsConfig->tableName);
		elektraFree (dsConfig->keyColName);
		elektraFree (dsConfig->valColName);

		elektraFree (dsConfig->metaTableName);
		elektraFree (dsConfig->metaTableKeyColName);
		elektraFree (dsConfig->metaTableMetaKeyColName);
		elektraFree (dsConfig->metaTableMetaValColName);
	}

	elektraFree (dsConfig);
}

/**
 * @brief Cleanup and free the struct for the shared data, and (if enabled) the inside struct with the data source config
 * and optionally also the strings it contains.
 *
 * If the connection handle is not NULL, the function disconnects and frees the connection handle.
 * If the environment handle is not NULL, the function frees the environment handle.
 *
 * @param sharedData The struct of type odbcSharedData
 * @param freeDsConfig 'true' if the dataSourceConfig struct inside @p sharedData should be freed too
 * @param freeDsConfigStrings only considered, if @p freeDsConfig is 'true', then it specifies if the strings
 * 	inside the dataSourceConfig struct should be freed too.
 *
 * @retval false if a NULL pointer was passed for @p sharedData or an error was reported by ODBC when trying to disconnect or free
 * 	the handles.
 * @retval true otherwise.
 */
bool clearOdbcSharedData (struct odbcSharedData * sharedData, bool freeDsConfig, bool freeDsConfigStrings)
{
	bool finalRet = true;

	if (!sharedData)
	{
		return false;
	}

	if (sharedData->connection)
	{
		SQLRETURN ret = SQLDisconnect (sharedData->connection);

		if (!SQL_SUCCEEDED (ret))
		{
			finalRet = false;
		}

		ret = SQLFreeHandle (SQL_HANDLE_DBC, sharedData->connection);

		if (!SQL_SUCCEEDED (ret))
		{
			finalRet = false;
		}

		sharedData->connection = NULL;
	}

	if (sharedData->environment)
	{
		if (!SQL_SUCCEEDED (SQLFreeHandle (SQL_HANDLE_ENV, sharedData->environment)))
		{
			finalRet = false;
		}

		sharedData->environment = NULL;
	}

	if (freeDsConfig)
	{
		clearDsConfig (sharedData->dsConfig, freeDsConfigStrings);
	}

	return finalRet;
}


/**
 * @brief Close the connection and free handles for connection and environment
 * @param plugin The plugin that contains the odbcSharedData struct which should be cleared
 * @param[out] errorKey Used to add warnings.
 *
 * @retval true if the plugin data was NULL or the ODBC handles could be freed successfully
 * @retval false if an error occurred
 */
bool freeSharedHandles (Plugin * plugin, Key * errorKey)
{
	struct odbcSharedData * sharedData = elektraPluginGetData (plugin);
	if (!clearOdbcSharedData (sharedData, false, false))
	{
		sharedData->connection = NULL;
		sharedData->environment = NULL;
		ELEKTRA_ADD_RESOURCE_WARNING (errorKey,
					      "Could not successfully close the connection and free the SQL handles for "
					      " the connection and environment. Please check the state of your data source.");
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	else
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
}

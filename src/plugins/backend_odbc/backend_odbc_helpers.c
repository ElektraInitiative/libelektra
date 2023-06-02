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
#include <string.h>

/**
 * @brief Get the number of digits an integer value consists of (e.g. 123 has 3 digits, therefore 3 is returned)
 *
 * @param i The integer value from which the number of digits should be computed
 *
 * @return The number of digits the given integer value consists of
 */
unsigned char getNumDigits (int i)
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
 * 	environment = SQL_HANDLE_ENV
 * 	connection = SQL_HANDLE_DBC
 * 	statement = SQL_HANDLE_STMT
 * 	descriptor = SQL_HANDLE_DESC
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
	int msgCount = 0;

	/* two iterations, 1st to determine length of string, 2nd for actually creating the string */
	for (int i = 0; i < 2; i++)
	{
		if (i)
		{
			ELEKTRA_ASSERT (msgCount >= 0,
					"The message counter reached a value which indicates a negative count of messages, this looks like "
					"a bug!\nPlease report this bug at https://issues.libelektra.org.");

			if (msgCount == 0)
			{
				return NULL;
			}

			text = elektraMalloc (maxLenText + 1);
			resultArray = elektraMalloc ((msgCount + 1) * sizeof (char *));
		}

		SQLINTEGER recNum = 1;
		for (msgCount = 0; SQL_SUCCEEDED (SQLGetDiagRec (handleType, odbcHandle, recNum++, state, &nativeErrCode, text,
								 (i ? maxLenText + 1 : 0), &lenText));
		     msgCount++)
		{
			if (i)
			{
				/* 5 extra chars (3x ':', 1x i, 1x '\0') */
				resultArray[msgCount] = elektraMalloc (SQL_SQLSTATE_SIZE + maxLenText + getNumDigits (nativeErrCode) + 5);
				sprintf (resultArray[msgCount], "%s:%d:%d:%s", state, i, nativeErrCode, text);
			}
			else
			{
				if (lenText > maxLenText) maxLenText = lenText;
			}
		}

		if (i)
		{
			elektraFree (text);
			return resultArray;
		}
	}

	return NULL;
}


/**
 * @brief Set an ODBC error and/or add ODBC warnings to @p errorKey
 *
 * @param handleType The type of the handle (environment, connection, statement, descriptor)
 * 	environment = SQL_HANDLE_ENV
 * 	connection = SQL_HANDLE_DBC
 * 	statement = SQL_HANDLE_STMT
 * 	descriptor = SQL_HANDLE_DESC
 * @param handle The actual valid ODBC handle which must match the type given in @p handleType
 * @param functionName The name of the function in which the error/warning(s) were emitted
 * @param isWarning true: treat first record as warning, false: treat first record as error
 * @param errorKey Used to store errors and warnings
 *
 * @return The number of processed errors/warnings
 * @retval 0 if no error/warning was found
 * @retval -1 if an error occurred in this function (e.g. invalid handle given)
 */
int setOdbcError (SQLSMALLINT handleType, SQLHANDLE handle, char * functionName, bool isWarning, Key * errorKey)
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

		/* Only one error can be set, all other ODBC errors are added as warnings
		 * According to the definition, records that describe errors come first
		 * see: https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/sequence-of-status-records */
		if (i == 1 && !isWarning)
		{
			ELEKTRA_SET_INTERNAL_ERRORF (errorKey,
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
		else
		{
			ELEKTRA_ADD_INTERNAL_WARNINGF (errorKey,
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

	SQLCHAR * dsn = NULL;
	SQLSMALLINT lenDsn;
	SQLSMALLINT maxLenDsn = 0;

	int dsnCount;
	char ** result = NULL;

	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env);
	SQLSetEnvAttr (env, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) SQL_OV_ODBC3, 0);

	/* 2 runs, 1st for determining needed string buffer sizes, 2nd for actually retrieving the strings */
	for (int i = 0; i < 2; i++)
	{
		if (i)
		{
			ELEKTRA_ASSERT (dsnCount >= 0,
					"The datasource counter reached a value which indicates a negative count of data sources, this "
					"looks like a bug!\nPlease report this bug at https://issues.libelektra.org.");
			if (dsnCount == 0)
			{
				/* No data sources found */
				return NULL;
			}

			/* We need one entry for the NULL indicating the end of the returned string array */
			result = elektraMalloc ((dsnCount + 1) * sizeof (char *));

			/* Add one byte for \0 */
			dsn = elektraMalloc ((maxLenDsn + 1) * sizeof (SQLCHAR));
		}

		dsnCount = 0;
		for (SQLUSMALLINT direction = SQL_FETCH_FIRST;
		     SQL_SUCCEEDED (SQLDataSources (env, direction, dsn, i ? maxLenDsn + 1 : 0, &lenDsn, NULL, 0, NULL));
		     direction = SQL_FETCH_NEXT)
		{
			if (i)
			{
				result[dsnCount] = (char *) dsn;
			}
			else
			{
				if (lenDsn > maxLenDsn) maxLenDsn = lenDsn;
			}
			dsnCount++;
		}
	}

	SQLFreeHandle (SQL_HANDLE_ENV, env);
	return result;
}


/**
 * @brief Lookup the string value of a @b Key in a @b KeySet
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

	char * resultString = elektraMalloc (keyGetValueSize (resultKey));

	if (!resultString)
	{
		return NULL;
	}

	ssize_t ret = keyGetString (resultKey, resultString, keyGetValueSize (resultKey));
	ELEKTRA_ASSERT (ret != 0,
			"keyGetString() returned 0! This should not have happened! Please report the bug at https://issues.libelektra.org");

	if (ret == -1)
	{
		elektraFree (resultString);
		return NULL;
	}

	return resultString;
}

/**
 * @brief Creates a struct for the data source configuration that is provided in a @b KeySet
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
struct dataSourceConfig * fillDsStructFromDefinitionKs (KeySet * ksDefinition)
{
	if (!ksDefinition)
	{
		return NULL;
	}

	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	if (!dsConfig)
	{
		return NULL;
	}

	/* Get configuration data for the ODBC data source */
	bool valueMissing = false;

	dsConfig->dataSourceName = lookupStringFromKs (ksDefinition, "system:/dataSourceName");

	if (!(dsConfig->dataSourceName))
	{
		valueMissing = true;
	}

	if (!valueMissing)
	{
		/* Username and password are optional (NULL = no username/password)
		 * Some data sources don't require credentials or have them configured as part of the ODBC data source definition
		 * (e.g. in odbc.ini)
		 */
		dsConfig->userName = lookupStringFromKs (ksDefinition, "system:/userName");
		dsConfig->password = lookupStringFromKs (ksDefinition, "system:/password");

		dsConfig->tableName = lookupStringFromKs (ksDefinition, "system:/table/name");
		if (!(dsConfig->tableName))
		{
			valueMissing = true;
		}
	}

	if (!valueMissing)
	{
		dsConfig->keyColName = lookupStringFromKs (ksDefinition, "system:/table/keyColName");
		dsConfig->valColName = lookupStringFromKs (ksDefinition, "system:/table/valColName");

		if (!(dsConfig->keyColName) || !(dsConfig->valColName))
		{
			valueMissing = true;
		}
	}

	if (!valueMissing)
	{
		dsConfig->metaTableName = lookupStringFromKs (ksDefinition, "system:/metaTable/name");

		if (dsConfig->metaTableName)
		{
			dsConfig->metaTableKeyColName = lookupStringFromKs (ksDefinition, "system:/metaTable/keyColName");
			dsConfig->metaTableMetaKeyColName = lookupStringFromKs (ksDefinition, "system:/metaTable/metaKeyColName");
			dsConfig->metaTableMetaValColName = lookupStringFromKs (ksDefinition, "system:/metaTable/metaValColName");

			if (!(dsConfig->metaTableKeyColName) || !(dsConfig->metaTableMetaKeyColName) ||
			    !(dsConfig->metaTableMetaValColName))
			{
				valueMissing = true;
			}
		}
	}


	if (valueMissing)
	{
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
		dsConfig = NULL;
	}

	return dsConfig;
}

/**
 * @brief Creates a string the contains all mandatory configuration values for an ODBC data source
 *
 * The created string is intended for use as an identifier for a data source.
 * This function should be used during the @b RESOLVER phase of the ODBC backend.
 *
 * @param dsConfig A valid data source config, as returned by the \ref fillDsStructFromDefinitionKs() function
 *
 * @return A string that contains all fields of the config that identify a data source for the ODBC backend, copied to newly allocated
 * memory Make sure to free the returned string.
 *
 * @retval NULL If an empty configuration struct was passed, no data source name was found or memory allocation failed
 *
 * @see 'fillDsStructFromDefinitionKs(KeySet * ksDefinition)' to get a dataSourceConfig struct from a KeySet with the mountpoint definition
 */
char * dsConfigToString (struct dataSourceConfig * dsConfig)
{
	if (!dsConfig || !dsConfig->dataSourceName || !(*(dsConfig->dataSourceName)))
	{
		return NULL;
	}

	/* add one char for the \0 and 3 char for separating the values with ' - ' */
	size_t dsConfigStrLen = 1 + (dsConfig->dataSourceName ? strlen (dsConfig->dataSourceName) : 0) +
				+(dsConfig->tableName ? 3 + strlen (dsConfig->tableName) : 0) +
				+(dsConfig->keyColName ? 3 + strlen (dsConfig->keyColName) : 0) +
				+(dsConfig->valColName ? 3 + strlen (dsConfig->valColName) : 0) +
				+(dsConfig->metaTableName ? 3 + strlen (dsConfig->metaTableName) : 0) +
				+(dsConfig->metaTableKeyColName ? 3 + strlen (dsConfig->metaTableKeyColName) : 0) +
				+(dsConfig->metaTableMetaKeyColName ? 3 + strlen (dsConfig->metaTableMetaKeyColName) : 0) +
				+(dsConfig->metaTableMetaValColName ? 3 + strlen (dsConfig->metaTableMetaValColName) : 0);

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

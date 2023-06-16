/**
 * @file
 *
 * @brief Functions for getting data from an ODBC data source.
 *
 * This file contains all functions that are especially needed for retrieving data from a data source.
 * This includes building a string for a SELECT query, preparing and executing select queries and fetching data.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./backend_odbc_get.h"
#include "./backend_odbc_general.h"

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <string.h>

/* ODBC related includes */
#include <sql.h>
#include <sqlext.h>

/**
 * @internal
 *
 * @brief Constructs a SELECT query string for the ODBC backend based on the given data source configuration.
 *
 * @pre All mandatory values in @p dsConfig must be present and valid.
 *
 * @param dsConfig A valid data source config, as returned by the fillDsStructFromDefinitionKs() function
 * @param quoteString The characters that should be added before and after identifiers, pass NULL if your identifiers in dsConfig are
 * 	already quoted or if you don't want to use quoted identifiers
 * @param[out] errorKey Used to store errors and warnings.
 *
 * @return The query string for the select query to get keynames, key-values and metadata from an SQL data source.
 * 	Make sure to free the returned string.
 * @retval NULL if memory allocation failed, no @p quoteString was given or an invalid identifier value
 * 	was in @p dsConfig, see warnings and error in @p errorKey for more details
 */
static char * getSelectQueryString (const struct dataSourceConfig * dsConfig, char * quoteString, Key * errorKey)
{
	/* A sample query string that shows the structure of the SELECT query that this function generates:
	 * SELECT "elektra"."key", "elektra"."val", "elektrameta"."metakey", "elektrameta"."metaval" FROM {oj "elektra" LEFT OUTER JOIN
	 * "elektrameta" ON "elektra"."key"="elektrameta"."key"} ORDER BY "elektra"."key".
	 */

	if (!quoteString || !(*quoteString))
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL or empty strings are not supported for the quote string.");
		return NULL;
	}

	/* Check if any identifier contains the quote string */
	if (checkIdentifiersForSubString (dsConfig, quoteString, errorKey))
	{
		/* A concrete error message should have been set to errorKey */
		return NULL;
	}

	// clang-format off
	char * queryString = elektraFormat ("SELECT %s%s%s.%s%s%s, %s%s%s.%s%s%s, %s%s%s.%s%s%s, %s%s%s.%s%s%s FROM {oj %s%s%s "
		"LEFT OUTER JOIN %s%s%s ON %s%s%s.%s%s%s=%s%s%s.%s%s%s} ORDER BY %s%s%s.%s%s%s",
		quoteString, dsConfig->tableName, quoteString,
		quoteString, dsConfig->keyColName, quoteString,

		quoteString, dsConfig->tableName, quoteString,
		quoteString, dsConfig->valColName, quoteString,

		quoteString, dsConfig->metaTableName, quoteString,
		quoteString, dsConfig->metaTableMetaKeyColName, quoteString,

		quoteString, dsConfig->metaTableName, quoteString,
		quoteString, dsConfig->metaTableMetaValColName, quoteString,

		quoteString, dsConfig->tableName, quoteString,

		quoteString, dsConfig->metaTableName, quoteString,

		quoteString, dsConfig->tableName, quoteString,
		quoteString, dsConfig->keyColName, quoteString,

		quoteString, dsConfig->metaTableName, quoteString,
		quoteString, dsConfig->metaTableKeyColName, quoteString,

		quoteString, dsConfig->tableName, quoteString,
		quoteString, dsConfig->keyColName, quoteString);
	// clang-format on


	if (!queryString)
	{
		return NULL;
	}

	return queryString;
}


/**
 * @internal
 *
 * @brief Prepares a SELECT SQL-statement that can later be executed to actually fetch the values.
 *
 * The statement is constructed to retrieve all keys, values and associated metadata.
 *
 * @pre The @p sqlConnection handle must have been initialized and a connection must have been established.
 * @pre All mandatory values in @p dsConfig must be present and valid.
 *
 * @param sqlConnection The initialized connection handle. It must represent an active connection.
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned NULL.
 * @param dsConfig The configuration of the ODBC data source, as retrieved by fillDsStructFromDefinitionKs()
 * @param[out] errorKey Used to store errors and warnings.
 *
 * @return A handle to the prepared statement
 * 	Make sure to free the returned handle with SQLFreeHandle().
 * @retval NULL if an error occurred (see @p errorKey for details)
 *
 * @see fillDsStructFromDefinitionKs() for getting a valid dataSourceConfig struct
 */
static SQLHSTMT prepareSelectStmt (SQLHDBC sqlConnection, const struct dataSourceConfig * dsConfig, Key * errorKey)
{
	/* Handle for a statement */
	SQLHSTMT sqlStmt = allocateStatementHandle (sqlConnection, errorKey);

	if (!sqlStmt)
	{
		return NULL;
	}

	char * identifierQuoteStr = getQuoteStr (sqlConnection, errorKey);
	char * queryString = getSelectQueryString (dsConfig, identifierQuoteStr, errorKey);
	elektraFree (identifierQuoteStr);

	if (!queryString || !(*queryString))
	{
		if (!queryString)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		}
		else
		{
			elektraFree (queryString);
			ELEKTRA_SET_INSTALLATION_ERROR (errorKey,
							"The input configuration for the data source contained missing or invalid values.\n"
							"Please check your ODBC- and data source configuration!");
		}

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return NULL;
	}

	/* Prepare the statement that retrieves key-names and string-values */
	SQLRETURN ret = SQLPrepare (sqlStmt, (SQLCHAR *) queryString, SQL_NTS);

	/* Not a deferred buffer, according to: https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/deferred-buffers */
	elektraFree (queryString);


	if (SQL_SUCCEEDED (ret))
	{
		if (ret == SQL_SUCCESS_WITH_INFO)
		{
			ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, sqlStmt, errorKey);
		}

		return sqlStmt;
	}
	else
	{
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate a handle for an SQL statement!");
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_STMT, sqlStmt, errorKey);

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return NULL;
	}
}


/**
 * @internal
 *
 * @brief Binds the buffers in the struct @p columnData to the columns defined by the @p sqlStmt
 *
 * This function assumes that there are four columns ('key-name', 'key-value', 'metakey-name' and 'metakey-value')
 *
 * @param sqlStmt The allocated and prepared SQL statement for which the columns should be bound
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned 'false'.
 * @param buffers The buffers where the data for that columns should be stored
 * @param[out] errorKey Used to store errors and warnings
 *
 * @retval 'true' if the operation was successful
 * @retval 'false' if an error occurred (see @p errorKey for details)
 *
 * @see prepareSelectStmt() for getting a statement handle that can be used by this function
 */
static bool bindColumns (SQLHSTMT sqlStmt, struct columnData * buffers, Key * errorKey)
{
	/* TODO: Support for binary keys */

	/* Bind column 1 (key-name) */
	SQLRETURN ret = SQLBindCol (sqlStmt, 1, SQL_C_CHAR, buffers->bufferKeyName, KEYNAME_BUFFER_SIZE, &(buffers->nameLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the first column to the keyName!");
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_STMT, sqlStmt, errorKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, sqlStmt, errorKey);
	}

	/* Bind column 2 (key-value) */
	ret = SQLBindCol (sqlStmt, 2, SQL_C_CHAR, buffers->bufferKeyStr, KEYSTRING_BUFFER_SIZE, &(buffers->strLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_STMT, sqlStmt, errorKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, sqlStmt, errorKey);
	}

	/* Bind column 3 (metakey-name) */
	ret = SQLBindCol (sqlStmt, 3, SQL_C_CHAR, buffers->bufferMetaKeyName, METAKEYNAME_BUFFER_SIZE, &(buffers->metaNameLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_STMT, sqlStmt, errorKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, sqlStmt, errorKey);
	}

	/* Bind column 4 (metakey-value) */
	ret = SQLBindCol (sqlStmt, 4, SQL_C_CHAR, buffers->bufferMetaKeyStr, METASTRING_BUFFER_SIZE, &(buffers->metaStrLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_STMT, sqlStmt, errorKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, sqlStmt, errorKey);
	}

	return true;
}


/**
 * @internal
 *
 * @brief Executes a prepared SQL statement.
 *
 * @pre The @p sqlStmt handle must have been initialized and prepared
 *
 * @param sqlStmt The prepared SQL statement that should be executed
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned 'false'.
 * @param[out] errorKey Used to store errors and warnings
 *
 * @retval 'true' if the statement got executed successfully
 * @retval 'false' if an error occurred (see @p errorKey for details)
 *
 * @see prepareSelectStmt() for getting a valid and prepared SELECT statement that can be used by this function
 */
static bool executeSqlStatement (SQLHSTMT sqlStmt, Key * errorKey)
{
	SQLRETURN ret = SQLExecute (sqlStmt);

	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_STMT, sqlStmt, errorKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, sqlStmt, errorKey);
	}

	return true;
}

/**
 * @internal
 *
 * @brief Use this function to retrieve data that didn't fit into the pre-defined buffers.
 *
 *
 * This function is used to retrieve data that gets returned by the execution of a SELECT-statement and didn't fit into the
 * fix-sized buffers. For such values, memory must be allocated dynamically.
 *
 * The function also re-allocates the buffer if it was still too small to get the whole returned data.
 * This is especially useful for data sources that don't know the size of the returned data in advance.
 *
 * @pre The buffer @p targetValue must point to allocated memory of size @p bufferSize.
 * Please consider that the buffer should be of type char*, like common strings, not of type char**, like string-arrays.
 * The buffer is just passed as a pointer (char**) because the memory maybe gets reallocated and moved to a different memory address.
 *
 * @param sqlStmt The executed statement, for which SQLFetch() has been called before
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned 'false'.
 * @param colNumber The number of the column from which the data should be retrieved
 * @param targetType The C data type that the fetched result should be converted to
 * 	See https://learn.microsoft.com/en-us/sql/odbc/reference/appendixes/converting-data-from-c-to-sql-data-types for supported
 * conversions.
 * @param[in,out] targetValue The buffer where the data should be stored
 * @param bufferSize The size of the buffer (as given to malloc())
 * @param[out] errorKey Used to store errors and warnings
 *
 * @rerval 'true' if the data was fetched successfully
 * @retval 'false' if an error occurred (see @p errorKey for details)
 *
 * @note The row number is not given as an argument, but related to the last call of SQLFetch()
 *
 * @see fetchResults() for processing the results of an executed SELECT statement
 */
static bool getLongData (SQLHSTMT sqlStmt, SQLUSMALLINT colNumber, SQLSMALLINT targetType, char ** targetValue, SQLLEN bufferSize,
			 Key * errorKey)
{
	SQLRETURN getDataRet;
	unsigned int iteration = 0;

	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "A NULL pointer was given as an argument for 'sqlStmt' to getLongData()");
		return false;
	}
	else if (!targetValue)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey,
					     "A NULL pointer was given as an argument for the buffer 'targetValue' to getLongData()");
		return false;
	}
	else if (bufferSize <= 0)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "A buffer size <=0 was given as an argument to getLongData()");
		return false;
	}

	do
	{
		if (iteration > 0)
		{
			if (elektraRealloc ((void **) targetValue, bufferSize * (iteration + 1)) == -1)
			{
				SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
				ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
				return false;
			}
		}

		getDataRet = SQLGetData (sqlStmt, colNumber, targetType, (*targetValue) + (iteration * (bufferSize - 1)), bufferSize, NULL);

		if (SQL_SUCCEEDED (getDataRet))
		{
			if (getDataRet == SQL_SUCCESS_WITH_INFO)
			{
				ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, sqlStmt, errorKey);
			}
			else
			{
				/* The last call returns SQL_SUCCESS, no more iterations necessary
				 * see: https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/getting-long-data */
				return true;
			}
		}
		else
		{
			ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_STMT, sqlStmt, errorKey);
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			return false;
		}

		iteration++;

	} while (getDataRet != SQL_NO_DATA);

	return true;
}

/**
 * @internal
 *
 * @brief Fetch the data that was queried by executing a SELECT statement.
 *
 * @pre The @p sqlStmt must have been executed before calling this function with it as an argument.
 * This function also converts the data retrieved from the datasource to a KeySet (keys, associated values and metadata)
 *
 * @param sqlStmt An executed SQL statement
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned NULL.
 * @param buffers A struct with the pre-defined output buffers that are used to retrieve that data of the queried columns
 * 	See 'struct columnData'.
 * @param[out] parentKey Used to store errors and warnings and for getting the mountpoint root path
 *
 * @returns The KeySet with the data retrieved from the SQL SELECT query.
 * 	Make sure to ksDel() the returned KeySet.
 * @retval NULL if no data was fetched or an error occurred (see @p errorKey for details)
 *
 * @see executeSqlStatement() for executing a prepared SQL statement
 */
static KeySet * fetchResults (SQLHSTMT sqlStmt, struct columnData * buffers, Key * parentKey)
{
	SQLRETURN ret;
	KeySet * ksResult = NULL;

	/* Needed for detecting if the current row contains a new key or just a further metakey for a previous key (outer join) */
	char * prevKeyName = NULL;
	Key * curKey = NULL;
	Key * prevKey = NULL;

	if (!sqlStmt || !buffers)
	{
		if (parentKey)
		{
			ELEKTRA_SET_INTERFACE_ERROR (
				parentKey, "A NULL pointer was given as an argument to fetchResults() for 'sqlStmt' or 'columnData'");
		}
		return NULL;
	}

	while ((ret = SQLFetch (sqlStmt)) != SQL_NO_DATA)
	{

		if (!SQL_SUCCEEDED (ret))
		{
			ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_STMT, sqlStmt, parentKey);
			elektraFree (prevKeyName);

			if (ksResult)
			{
				ksDel (ksResult);
			}

			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			return NULL;
		}

		ELEKTRA_ASSERT (buffers->bufferKeyName[0] && buffers->nameLenInd != SQL_NULL_DATA,
				"The ODBC-backend retrieved an empty- or null-string for the key-name. This is not allowed! Please check "
				"you data source.");

		char * longKeyName = NULL;
		char * longKeyString = NULL;
		char * longMetaKeyName = NULL;
		char * longMetaKeyString = NULL;
		bool isFurtherMetaKey = false;


		if (ret == SQL_SUCCESS_WITH_INFO)
		{
			/* Check if string was truncated (buffer too small) */
			SQLCHAR sqlState[SQL_SQLSTATE_SIZE + 1];
			SQLGetDiagField (SQL_HANDLE_STMT, sqlStmt, 1, SQL_DIAG_SQLSTATE, sqlState, SQL_SQLSTATE_SIZE + 1, 0);


			/* SQLSTATE 01004 = Data truncated */
			if (elektraStrCmp ((const char *) sqlState, "01004") == 0)
			{
				/* The buffer size constants include the byte for the \0, while the LenInd-variables don't! */

				/* Check key-name column */
				bool retGetLongData = true;
				if (buffers->nameLenInd == SQL_NO_TOTAL)
				{
					/* No information about the total length is available --> The new buffer maybe must be
					 * resized multiple times */
					longKeyName = (char *) elektraMalloc (sizeof (char) * (KEYNAME_BUFFER_SIZE * 2));
					retGetLongData =
						getLongData (sqlStmt, 1, SQL_C_CHAR, &longKeyName, KEYNAME_BUFFER_SIZE * 2, parentKey);
				}
				else if (KEYNAME_BUFFER_SIZE <= buffers->nameLenInd)
				{
					longKeyName = (char *) elektraMalloc (sizeof (char) * (buffers->nameLenInd + 1));
					retGetLongData =
						getLongData (sqlStmt, 1, SQL_C_CHAR, &longKeyName, (buffers->nameLenInd + 1), parentKey);
				}

				/* Check if the keyname changed since the last iteration */
				if (longKeyName && prevKeyName && elektraStrCmp (longKeyName, prevKeyName) == 0)
				{
					elektraFree (longKeyName);
					longKeyName = NULL;
					isFurtherMetaKey = true;
				}
				else if (longKeyName)
				{
					elektraFree (prevKeyName);
					prevKeyName = longKeyName;
				}

				/* Check key-string column */
				if (!isFurtherMetaKey && retGetLongData && buffers->strLenInd == SQL_NO_TOTAL)
				{
					longKeyString = (char *) elektraMalloc (sizeof (char) * (KEYSTRING_BUFFER_SIZE * 2));
					retGetLongData =
						getLongData (sqlStmt, 2, SQL_C_CHAR, &longKeyString, KEYSTRING_BUFFER_SIZE * 2, parentKey);
				}
				else if (!isFurtherMetaKey && KEYSTRING_BUFFER_SIZE <= buffers->strLenInd)
				{
					longKeyString = (char *) elektraMalloc (sizeof (char) * (buffers->strLenInd + 1));
					retGetLongData =
						getLongData (sqlStmt, 2, SQL_C_CHAR, &longKeyString, (buffers->strLenInd + 1), parentKey);
				}

				/* Check metakey-name column */
				if (retGetLongData && buffers->metaNameLenInd == SQL_NO_TOTAL)
				{
					longMetaKeyName = (char *) elektraMalloc (sizeof (char) * (METAKEYNAME_BUFFER_SIZE * 2));
					retGetLongData = getLongData (sqlStmt, 3, SQL_C_CHAR, &longMetaKeyName, METAKEYNAME_BUFFER_SIZE * 2,
								      parentKey);
				}
				else if (METAKEYNAME_BUFFER_SIZE <= buffers->metaNameLenInd)
				{
					longMetaKeyName = (char *) elektraMalloc (sizeof (char) * (buffers->metaNameLenInd + 1));
					retGetLongData = getLongData (sqlStmt, 3, SQL_C_CHAR, &longMetaKeyName,
								      (buffers->metaNameLenInd + 1), parentKey);
				}

				/* Check metakey-string column */
				if (retGetLongData && buffers->metaStrLenInd == SQL_NO_TOTAL)
				{
					longMetaKeyString = (char *) elektraMalloc (sizeof (char) * (METASTRING_BUFFER_SIZE * 2));
					retGetLongData = getLongData (sqlStmt, 4, SQL_C_CHAR, &longMetaKeyString,
								      METASTRING_BUFFER_SIZE * 2, parentKey);
				}
				else if (METASTRING_BUFFER_SIZE <= buffers->metaStrLenInd)
				{
					longMetaKeyString = (char *) elektraMalloc (sizeof (char) * (buffers->metaStrLenInd + 1));
					retGetLongData = getLongData (sqlStmt, 4, SQL_C_CHAR, &longMetaKeyString,
								      (buffers->metaStrLenInd + 1), parentKey);
				}

				if (!retGetLongData)
				{
					/* The variable longKeyName should not be freed here, it either points to the same memory as
					 * prevKeyName or is NULL */

					elektraFree (prevKeyName);
					elektraFree (longKeyString);
					elektraFree (longMetaKeyName);
					elektraFree (longMetaKeyString);

					/* Statement handle was already freed by getLongData() function */
					if (ksResult)
					{
						ksDel (ksResult);
					}

					SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
					return NULL;
				}
			}
			else
			{
				ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, sqlStmt, parentKey);
			}
		}

		if (!isFurtherMetaKey && !longKeyName)
		{
			/* Check short keyname */
			if (prevKeyName && elektraStrCmp (prevKeyName, (const char *) buffers->bufferKeyName) == 0)
			{
				isFurtherMetaKey = true;
			}
			else
			{
				elektraFree (prevKeyName);
				prevKeyName = elektraStrDup ((const char *) buffers->bufferKeyName);
			}
		}

		if (!ksResult)
		{
			ksResult = ksNew (0, KS_END);
		}

		if (!isFurtherMetaKey)
		{
			/* Create new key */
			prevKey = curKey;
			curKey = keyDup (parentKey, KEY_CP_NAME);

			if (longKeyName)
			{
				keyAddName (curKey, longKeyName);
				/* No freeing of longKeyName here, because the same string is referenced by prevKeyName! */
			}
			else
			{
				keyAddName (curKey, (char *) buffers->bufferKeyName);
			}


			if (longKeyString)
			{
				keySetString (curKey, longKeyString);
				elektraFree (longKeyString);
			}
			else if (buffers->bufferKeyStr[0] && buffers->strLenInd != SQL_NULL_DATA)
			{
				keySetString (curKey, (char *) buffers->bufferKeyStr);
			}
			else
			{
				keySetString (curKey, "");
			}
		}
		else
		{
			ELEKTRA_ASSERT (curKey,
					"The flag the indicates that a further metakey is present was set, but the current key was NULL. "
					"This is likely a programming error.\nPlease report this issues at https://issues.libelektra.org");
		}

		const char * metaKeyName = NULL;

		if (longMetaKeyName)
		{
			metaKeyName = longMetaKeyName;
		}
		else if (buffers->bufferMetaKeyName[0] && buffers->metaNameLenInd != SQL_NULL_DATA)
		{
			metaKeyName = (const char *) buffers->bufferMetaKeyName;
		}
		else
		{
			ELEKTRA_ASSERT (!longMetaKeyString && !(buffers->bufferMetaKeyStr[0]),
					"No metakey-name was found, but a metakey-string, maybe the datasource contains invalid "
					"data, please check your datasource!");
		}


		if (metaKeyName)
		{
			if (longMetaKeyString || buffers->bufferMetaKeyStr[0])
			{
				if (longMetaKeyString)
				{
					keySetMeta (curKey, metaKeyName, longMetaKeyString);
					elektraFree (longMetaKeyString);
				}
				else
				{
					keySetMeta (curKey, metaKeyName, (const char *) buffers->bufferMetaKeyStr);
				}
			}
			else
			{
				keySetMeta (curKey, metaKeyName, "");
			}
		}

		elektraFree (longMetaKeyName);


		if (prevKey && curKey != prevKey)
		{
			/* Previous key (incl. all metakeys) is finished --> add it to KeySet */
			ksAppendKey (ksResult, prevKey);
		}

		/* Prepare buffers for next iteration */
		buffers->bufferKeyName[0] = 0;
		buffers->bufferKeyStr[0] = 0;
		buffers->bufferMetaKeyName[0] = 0;
		buffers->bufferMetaKeyStr[0] = 0;
	}

	elektraFree (prevKeyName);

	if (curKey && curKey != prevKey)
	{
		/* Add last key */
		ksAppendKey (ksResult, curKey);
	}

	if (!ksResult && sqlStmt)
	{
		/* The statement handle must be freed if the function returns NULL */
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
	}

	return ksResult;
}


/** @brief Use ODBC to get keys and values from a data source.
 *
 * @param dsConfig The configuration of the data source.
 * @param keepTransaction If true, autocommit is disabled, the started transaction is kept open and the environment and connection handles
 * 	are stored in @p sharedData.
 * @param[out] errorKey Used to store errors and warnings
 *
 * @returns A KeySet with Keys that represent the data that was fetched from the data source
 * 	Make sure to ksDel() the returned KeySet.
 * @retval NULL if an error occurred (see @p errorKey for details)
 */
KeySet * getKeysFromDataSource (struct odbcSharedData * sharedData, bool keepTransaction, Key * errorKey)
{
	if (!sharedData || !sharedData->dsConfig)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey,
					     "The provided data source configuration (struct odbcSharedData or the inner struct "
					     "dataSourceConfig) was NULL!");
		return NULL;
	}

	if (sharedData->environment && sharedData->connection)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey,
					     "The provided struct odbcSharedData must not contain SQL environment or connection "
					     "handles for the kdbGet(). These variables must be NULL pointers at this stage.");
		return NULL;
	}


	/* 1. Allocate Environment handle and register version */
	SQLHENV sqlEnv = allocateEnvHandle (errorKey);
	if (!sqlEnv)
	{
		return NULL;
	}

	/* Use ODBC version 3 */
	if (!setOdbcVersion (sqlEnv, SQL_OV_ODBC3, errorKey))
	{
		return NULL;
	}

	/* 2. Allocate connection handle, set timeout and enable autocommit */
	SQLHDBC sqlConnection = allocateConnectionHandle (sqlEnv, errorKey);
	if (!sqlConnection)
	{
		return NULL;
	}

	if (!setLoginTimeout (sqlConnection, sharedData->dsConfig->timeOut, errorKey))
	{
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	/* For the GET operation, only a single SELECT-query is made, therefore we don't need a rollback of transactions here,
	 * however, if you want to use a transaction and get the open transaction back, this is also supported and e.g. used by
	 * the kdbSet() function for the this ODBC backend plugin */
	if (!setAutocommit (sqlConnection, !keepTransaction, errorKey))
	{
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	/* 3. Connect to the datasource */
	if (!connectToDataSource (sqlConnection, sharedData->dsConfig, errorKey))
	{
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	/* 4. Create and prepare the SELECT query based on the given dsConfig */
	SQLHSTMT sqlStmt = prepareSelectStmt (sqlConnection, sharedData->dsConfig, errorKey);

	if (!sqlStmt)
	{
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	struct columnData outputBuffers = { .bufferKeyName[0] = 0,
					    .bufferKeyStr[0] = 0,
					    .bufferMetaKeyName[0] = 0,
					    .bufferMetaKeyStr[0] = 0,
					    .nameLenInd = 0,
					    .strLenInd = 0,
					    .metaNameLenInd = 0,
					    .metaStrLenInd = 0 };

	/* 5. Bind output columns to variables */
	if (!bindColumns (sqlStmt, &outputBuffers, errorKey))
	{
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
	}

	/* 6. Execute the query */
	if (!executeSqlStatement (sqlStmt, errorKey))
	{
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
	}

	/* 7. Fetch results */
	KeySet * ksResult = fetchResults (sqlStmt, &outputBuffers, errorKey);

	/* If no data could be fetched or errors occurred, the statement handle was already freed by the fetchResults()-function. */
	if (ksResult)
	{
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);

		if (keepTransaction)
		{
			sharedData->environment = sqlEnv;
			sharedData->connection = sqlConnection;
			return ksResult;
		}
	}

	SQLDisconnect (sqlConnection);
	ELEKTRA_LOG_DEBUG ("Disconnected from ODBC data source.");

	SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
	SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);

	return ksResult;
}

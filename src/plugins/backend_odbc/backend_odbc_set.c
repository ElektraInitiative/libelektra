/**
 * @file
 *
 * @brief Functions for writing data to an ODBC data source.
 *	INSERT-, UPDATE and DELETE operations are supported.
 *
 * This file contains all functions that are especially needed for setting data on a data source.
 * This includes building strings for INSERT INTO-, UPDATE- and DELETE-queries, preparing and executing queries and sending data.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "./backend_odbc_set.h"
#include "./backend_odbc_general.h"
#include "./backend_odbc_get.h"

#include <kdbassert.h>
#include <kdbdiff.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <string.h>

/* ODBC related includes */
#include <sqlext.h>


/* Value for SQLite, adjust for your ODBC driver/DBMS */
#define ODBC_SQL_MAX_PARAMS 32766


/**
 * @internal
 *
 * @brief Get an SQL query for deleting metadata for ONE specific key and some metakeys
 * 	If you want to delete ALL metakeys for one or more key-names, it's recommended to use the getDeleteQuery() function instead
 *
 * @param numMetaKeys The number of metakeys you want to remove (needed for number of parameters in IN(..) clause).
 * @param dsConfig The data source configuration (contains mountpoint definition).
 * @param quoteStr The char(s) that should be used for quoting identifiers.
 * 	The identifiers in @dsConfig must not contain @p quoteString as a substring.
 * @param[out] errorKey Used to store errors and warnings.
 *
 * @return The string with the query incl. parameter markers '?'. This query must be executed with a statement that has correctly bound
 * parameters. Make sure to free the returned string.
 * @retval NULL if an error occurred.
 */
static char * getDeleteMetaDataQuery (unsigned int numMetaKeys, const struct dataSourceConfig * dsConfig, const char * quoteStr,
				      Key * errorKey)
{
	if (!dsConfig)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "The provided dataSourceConfig struct must not be NULL.");
		return NULL;
	}

	if (!quoteStr || !(*quoteStr))
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL or empty strings are not supported for the quote string.");
		return NULL;
	}

	if (numMetaKeys == 0)
	{
		ELEKTRA_ADD_INTERFACE_WARNING (errorKey,
					       "The provided number of metakeys to delete was 0. "
					       "Therefore, no query was created.");
		return NULL;
	}

	/* Check if any of the used identifiers contains the quote string */
	if (strstr (dsConfig->metaTableName, quoteStr))
	{
		ELEKTRA_SET_INTERFACE_ERRORF (errorKey, "The meta table name contained the quote string '%s', this is not allowed!",
					      quoteStr);
		return NULL;
	}

	if (strstr (dsConfig->metaTableKeyColName, quoteStr))
	{
		ELEKTRA_SET_INTERFACE_ERRORF (errorKey,
					      "The specified name of the key column in the metatable contained the quote string "
					      "'%s', this is not allowed!",
					      quoteStr);
		return NULL;
	}

	if (strstr (dsConfig->metaTableMetaKeyColName, quoteStr))
	{
		ELEKTRA_SET_INTERFACE_ERRORF (errorKey,
					      "The specified name of the metakey column in the metatable contained the quote "
					      "string '%s', this is not allowed!",
					      quoteStr);
		return NULL;
	}

	/* Example: DELETE FROM metaTable WHERE keyName=myKey AND metaKeyName IN (mk1, mk2, mk3) */
	size_t queryStrLen = strlen ("DELETE FROM ") + strlen (quoteStr) + strlen (dsConfig->metaTableName) + strlen (quoteStr) +
			     strlen (" WHERE ") + strlen (quoteStr) + strlen (dsConfig->metaTableKeyColName) + strlen (quoteStr) +
			     strlen ("=? AND ") + strlen (quoteStr) + strlen (dsConfig->metaTableMetaKeyColName) + strlen (quoteStr) +
			     strlen (" IN ()");

	/* We need one comma less then number of metakeys to delete, so we already have to char for the \0 */
	queryStrLen += strlen ("?,") * numMetaKeys;

	char * queryStr = elektraMalloc (queryStrLen * sizeof (char));

	if (!queryStr)
	{
		return NULL;
	}

	char * strEnd = stpcpy (queryStr, "DELETE FROM ");
	strEnd = stpcpy (strEnd, quoteStr);
	strEnd = stpcpy (strEnd, dsConfig->metaTableName);
	strEnd = stpcpy (strEnd, quoteStr);
	strEnd = stpcpy (strEnd, " WHERE ");
	strEnd = stpcpy (strEnd, quoteStr);
	strEnd = stpcpy (strEnd, dsConfig->metaTableKeyColName);
	strEnd = stpcpy (strEnd, quoteStr);
	strEnd = stpcpy (strEnd, "=? AND ");
	strEnd = stpcpy (strEnd, quoteStr);
	strEnd = stpcpy (strEnd, dsConfig->metaTableMetaKeyColName);
	strEnd = stpcpy (strEnd, quoteStr);
	strEnd = stpcpy (strEnd, " IN (");

	for (unsigned int u = 0; u < numMetaKeys; u++)
	{
		if (u < (numMetaKeys - 1))
		{
			strEnd = stpcpy (strEnd, "?,");
		}
		else
		{
			/* last iteration */
			stpcpy (strEnd, "?)");
		}
	}

	return queryStr;
}


/**
 * @internal
 *
 * @brief Get an SQL query for deleting one or multiple keys from the key-table or ALL metadata for one or multiple keys from the metatable.
 *
 * @param numKeys The number of keys which you want to delete.
 * @param tableName The name of the table (key-table or metatable) from which you want to delete rows.
 * @param keyColName The name of the column that stores the key-name.
 * @param quoteStr The char(s) that should be used for quoting identifiers.
 * 	The identifiers in @dsConfig must not contain @p quoteString as a substring.
 * @param[out] errorKey Used to store errors and warnings.
 *
 * @return The string with the query incl. parameter markers '?'. This query must be executed with a statement
 * 	that has correctly bound parameters.
 * 	Make sure to free the returned string.
 */
static char * getDeleteQuery (unsigned int numKeys, const char * tableName, const char * keyColName, const char * quoteStr, Key * errorKey)
{
	if (numKeys == 0 || !tableName || !(*tableName) || !keyColName || !(*keyColName))
	{
		return NULL;
	}

	if (!quoteStr || !(*quoteStr))
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL or empty strings are not supported for the quote string.");
		return NULL;
	}

	/* Check if any of the used identifiers contains the quote string */
	if (strstr (tableName, quoteStr))
	{
		ELEKTRA_SET_INTERFACE_ERRORF (errorKey, "The table name contained the quote string '%s', this is not allowed!", quoteStr);
		return NULL;
	}

	if (strstr (keyColName, quoteStr))
	{
		ELEKTRA_SET_INTERFACE_ERRORF (errorKey,
					      "The specified name of the key column in the table contained the quote string "
					      "'%s', this is not allowed!",
					      quoteStr);
		return NULL;
	}

	/* Example: DELETE from myTable WHERE keyName IN (key1, key2, key3) */
	size_t queryStrLen = strlen ("DELETE FROM ") + strlen (quoteStr) + strlen (tableName) + strlen (quoteStr) + strlen (" WHERE ") +
			     strlen (quoteStr) + strlen (keyColName) + strlen (quoteStr) + strlen (" IN ()");

	/* One comma less than number of keys in the keyset is required, so we've already counted the char for \0 */
	queryStrLen += strlen ("?,") * numKeys;

	char * queryStr = elektraMalloc (queryStrLen * sizeof (char));

	if (!queryStr)
	{
		return NULL;
	}

	char * strEnd = stpcpy (queryStr, "DELETE FROM ");
	strEnd = stpcpy (strEnd, quoteStr);
	strEnd = stpcpy (strEnd, tableName);
	strEnd = stpcpy (strEnd, quoteStr);
	strEnd = stpcpy (strEnd, " WHERE ");
	strEnd = stpcpy (strEnd, quoteStr);
	strEnd = stpcpy (strEnd, keyColName);
	strEnd = stpcpy (strEnd, quoteStr);
	strEnd = stpcpy (strEnd, " IN (");

	for (unsigned int u = 0; u < numKeys; u++)
	{
		if (u < (numKeys - 1))
		{
			strEnd = stpcpy (strEnd, "?,");
		}
		else
		{
			/* last iteration */
			stpcpy (strEnd, "?)");
		}
	}
	return queryStr;
}


/**
 * @internal
 *
 * @brief Get an SQL query for updating an existing row (specified by key-name) with a new value.
 *
 * @param dsConfig The data source configuration (contains mountpoint definition).
 * @param forMetaTable Should a query for updating rows in the key-table or in the metatable be created?
 * @param quoteStr The char(s) that should be used for quoting identifiers.
 * 	The identifiers in @dsConfig must not contain @p quoteString as a substring.
 * @param[out] errorKey Used to store errors and warnings.
 *
 * @return The string with the query incl. parameter markers '?'. This query must be executed with a correctly prepared statement.
 * 	Make sure to free the returned string.
 */
static char * getUpdateQuery (const struct dataSourceConfig * dsConfig, bool forMetaTable, const char * quoteStr, Key * errorKey)
{
	if (!quoteStr || !(*quoteStr))
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "NULL or empty strings are not supported for the quote string.");
		return NULL;
	}

	/* Check if any identifier contains the quote string */
	if (checkIdentifiersForSubString (dsConfig, quoteStr, errorKey))
	{
		/* A concrete error message should have been set to errorKey */
		return NULL;
	}

	if (forMetaTable)
	{
		return elektraFormat ("UPDATE %s%s%s SET %s%s%s=? WHERE %s%s%s=? AND %s%s%s=?", quoteStr, dsConfig->metaTableName, quoteStr,
				      quoteStr, dsConfig->metaTableMetaValColName, quoteStr, quoteStr, dsConfig->metaTableKeyColName,
				      quoteStr, quoteStr, dsConfig->metaTableMetaKeyColName, quoteStr);
	}
	else
	{
		return elektraFormat ("UPDATE %s%s%s SET %s%s%s=? WHERE %s%s%s=?", quoteStr, dsConfig->tableName, quoteStr, quoteStr,
				      dsConfig->valColName, quoteStr, quoteStr, dsConfig->keyColName, quoteStr);
	}
}


/**
 * @internal
 *
 * @brief Get an SQL query for inserting a new row into the key-table or metatable.
 *
 * @param dsConfig The data source configuration (contains mountpoint definition).
 * @param forMetaTable Should a query for inserting rows in the key-table or in the metatable be created?
 * @param quoteStr The char(s) that should be used for quoting identifiers.
 * 	The identifiers in @dsConfig must not contain @p quoteString as a substring.
 * @param[out] errorKey Used to store errors and warnings.
 *
 * @return The string with the query incl. parameter markers '?'. This query must be executed with a statement that has correctly bound
 * parameters. Make sure to free the returned string.
 */
static char * getInsertQuery (const struct dataSourceConfig * dsConfig, bool forMetaTable, const char * quoteStr, Key * errorKey)
{
	/* Check if any identifier contains the quote string */
	if (checkIdentifiersForSubString (dsConfig, quoteStr, errorKey))
	{
		/* A concrete error message should have been set to errorKey */
		return NULL;
	}

	if (forMetaTable)
	{
		return elektraFormat ("INSERT INTO %s%s%s (%s%s%s, %s%s%s, %s%s%s) VALUES (?,?,?)", quoteStr, dsConfig->metaTableName,
				      quoteStr, quoteStr, dsConfig->metaTableKeyColName, quoteStr, quoteStr,
				      dsConfig->metaTableMetaKeyColName, quoteStr, quoteStr, dsConfig->metaTableMetaValColName, quoteStr);
	}
	else
	{
		/* Set value first to have the same parameter order as for the UPDATE query */
		return elektraFormat ("INSERT INTO %s%s%s (%s%s%s, %s%s%s) VALUES (?,?)", quoteStr, dsConfig->tableName, quoteStr, quoteStr,
				      dsConfig->valColName, quoteStr, quoteStr, dsConfig->keyColName, quoteStr);
	}
}


/**
 * @internal
 *
 * @brief Like bindStringsToStatement, just with a va_list argument instead of vararg ('...')
 *
 * @see bindStringsToStatement
 */
static bool bindStringsToStatementV (SQLHSTMT sqlStmt, ssize_t startPos, Key * errorKey, unsigned int numParams, va_list argList)
{
	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "The given statement handle must be allocated!");
		return false;
	}

	if (numParams == 0)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "The number of parameters to bind must be >0!");
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}

	if (startPos < 1)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (errorKey,
					      "The first parameter has the position 1, so the argument 'startPos' must be >= 1, "
					      "but you provided %zd",
					      startPos);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}

	if (numParams > USHRT_MAX)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (errorKey,
					      "The maximum number of parameter values to bind is %hu, but you specified a number of %u",
					      USHRT_MAX, numParams);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}

	for (unsigned short u = 0; u < (SQLUSMALLINT) numParams; u++)
	{
		SQLRETURN ret;
		const char * curStr = va_arg (argList, const char *);

		if (!curStr)
		{
			ELEKTRA_ADD_INTERFACE_WARNING (errorKey, "Binding NULL data to an SQL statement!");
			ret = SQLBindParameter (sqlStmt, u + startPos, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR, 0, 0, NULL, 0,
						(SQLLEN *) SQL_NULL_DATA);
		}
		else
		{
			ret = SQLBindParameter (sqlStmt, u + startPos, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR, strlen (curStr), 0,
						(SQLCHAR *) curStr, (SQLLEN) strlen (curStr) + 1, NULL);
		}

		ELEKTRA_LOG ("Bound value '%s' to statement\n", curStr);

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
	}

	return true;
}


/**
 * @internal
 *
 * @brief Bind a specified number of strings to a given SQL statement.
 *
 * The number of bound statements must equal the number of parameter markers ('?') in the query to execute.
 * You can combine binding strings and key-names from keys in a KeySet.
 * Just use the @p startPos argument to specify for which parameter(s) you want to bind the values.
 *
 * @param sqlStmt The statement for which the values should be bound.
 * @param startPos The position of the parameter marker ('?') for which the first given value is going to be bound.
 * @param[out] errorKey Used to store errors and warnings.
 * @param numParams The number of strings you want to bind.
 * @param ... The strings the should be bound.
 * 	Make sure that you only pass allocated char buffers that are not freed until the statement for which the values are bound is not
 * 	executed any more or other values are bound to the statement.
 *
 * @retval 'false' if an error occurred
 * @retval 'true' otherwise
 *
 * @see bindStringsToStatementV
 * @see bindKeyNamesToStatement
 */
static bool bindStringsToStatement (SQLHSTMT sqlStmt, ssize_t startPos, Key * errorKey, unsigned int numParams, ...)
{
	va_list argList;
	va_start (argList, numParams);
	bool ret = bindStringsToStatementV (sqlStmt, startPos, errorKey, numParams, argList);
	va_end (argList);
	return ret;
}


/**
 * @internal
 *
 * @brief Bind all key-names of the Keys in a KeySet to a statement.
 *
 * Make sure to bind exactly the number of parameters that match the number of the placeholder which the query-string you want to execute
 * what the given statement handle has.
 *
 * @param sqlStmt The statement for which the values should be bound.
 * @param keysToBind The KeySet that contains the Keys for which you want to bind the key-names.
 * @param startPosStatement The position of the parameter marker ('?') for which the first given value is going to be bound.
 *	The first parameter has the value '1', so a startPos <1 is an invalid value.
 * @param startPosKeySet The position of the first key in @p keyToBind that gets bound
 * 	The index of the first key has the value '0'.
 * @param endPosKeySet The position of the last key in @p keyToBind that gets bound
 * 	The index of the last key has the value ksGetSize(keysToBind)-1.
 * @param useRelativeKeyNames Should the relative names of the key-names be bound? This is the keyname without the beginning-part.
 * 	The key-name of @parentKey is used to determine the relative name.
 * 	Example: key-name = "user:/software/odbc", key-name of parent key = "user:/software" --> string "odbc" gets bound.
 * 	If set to 'false', the base-names of the Keys are used (esp. useful for processing rows in the metatable).
 *
 * @param[in,out] parentKey The key-name of this key is needed to determine the relative key-names of the Keys in the KeySet.
 * 	Used to store errors and warnings.
 *
 * @retval 'false' if an error occurred
 * @retval 'true' otherwise
 *
 * @see elektraKeyGetRelativeName
 * @see keyBaseName
 */
static bool bindKeyNamesToStatement (SQLHSTMT sqlStmt, const KeySet * keysToBind, ssize_t startPosStatement, ssize_t startPosKeySet,
				     ssize_t endPosKeySet, bool useRelativeKeyNames, Key * parentKey)
{
	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided statement handle must not be NULL.");
		return false;
	}

	if (startPosStatement < 1)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey,
					      "The first parameter has the position 1, so the argument 'startPosStatement' must be >= 1, "
					      "but you provided %zd",
					      startPosStatement);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}

	if (ksGetSize (keysToBind) < 1)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The KeySet with the keys to bind must not be NULL or empty.");
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}

	if ((endPosKeySet - startPosKeySet) >= USHRT_MAX)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey,
					      "The maximum number of parameter values to bind is %hu, but you specified a number of %zd",
					      USHRT_MAX, ksGetSize (keysToBind));
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}

	if (ksGetSize (keysToBind) <= endPosKeySet)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey,
					      "The given end position (%zd) is beyond the index of the last element (%zd) in the KeySet.",
					      endPosKeySet, ksGetSize (keysToBind) - 1);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}


	for (elektraCursor it = startPosKeySet; it <= endPosKeySet; it++)
	{
		const char * curKeyName = NULL;
		if (useRelativeKeyNames)
		{
			curKeyName = elektraKeyGetRelativeName (ksAtCursor (keysToBind, it), parentKey);
		}
		else
		{
			/* use basename for metakeys to remove "meta:/" */
			curKeyName = keyBaseName (ksAtCursor (keysToBind, it));
		}

		SQLRETURN ret = SQLBindParameter (sqlStmt, (it + startPosStatement) - startPosKeySet, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR,
						  strlen (curKeyName), 0, (SQLCHAR *) curKeyName, (SQLLEN) strlen (curKeyName) + 1, NULL);

		ELEKTRA_LOG ("Bound value '%s' to statement\n", curKeyName);

		if (!SQL_SUCCEEDED (ret))
		{
			ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_STMT, sqlStmt, parentKey);
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			return false;
		}
		else if (ret == SQL_SUCCESS_WITH_INFO)
		{
			ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, sqlStmt, parentKey);
		}
	}

	return true;
}


/**
 * @internal
 *
 * @brief Remove rows from a table in the data source.
 *
 * @param sqlStmt The statement on which the DELETE query should be executed.
 * @param ksRemovedKeys A KeySet with the Keys that should be removed (only the key-name is considered, the key-value is for determining
 * 	which rows should be deleted.
 * @param tableName The name of the table from which the rows should be removed.
 * @param keyColName The name of the column that contains the name of the Keys.
 * @param quoteStr The char(s) that should be used for quoting identifiers.
 * @param[in,out] parentKey The key-name of this key is needed to determine the relative key-names of the Keys in the KeySet.
 * 	Used to store errors and warnings.
 *
 * @return The number of the deleted rows.
 * @retval -1 if an error occurred.
 * @retval -2 it the number of deleted rows could not be retrieved (e.g. because the used ODBC driver doesn't support this feature).
 */
static SQLLEN removeRows (SQLHSTMT sqlStmt, const KeySet * ksRemovedKeys, const char * tableName, const char * keyColName,
			  const char * quoteStr, Key * parentKey)
{
	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided statement handle must not be NULL.");
		return -1;
	}

	ssize_t numRemovedKeys = ksGetSize (ksRemovedKeys);
	char * queryDelete = NULL;
	SQLLEN sumAffectedRows = 0;
	ssize_t endPos = -1;

	for (ssize_t startPos = 0; numRemovedKeys > 0; startPos = endPos + 1)
	{
		if (numRemovedKeys > ODBC_SQL_MAX_PARAMS)
		{
			if (!queryDelete)
			{
				queryDelete = getDeleteQuery (ODBC_SQL_MAX_PARAMS, tableName, keyColName, quoteStr, parentKey);
			}

			endPos += ODBC_SQL_MAX_PARAMS;
			numRemovedKeys -= ODBC_SQL_MAX_PARAMS;
		}
		else
		{
			if (queryDelete)
			{
				elektraFree (queryDelete);
			}

			SQLFreeStmt (sqlStmt, SQL_RESET_PARAMS);
			queryDelete = getDeleteQuery (numRemovedKeys, tableName, keyColName, quoteStr, parentKey);

			/* Last iteration */
			endPos += numRemovedKeys;
			numRemovedKeys = 0;
		}

		if (!queryDelete)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			return -1;
		}

		if (!bindKeyNamesToStatement (sqlStmt, ksRemovedKeys, 1, startPos, endPos, true, parentKey))
		{
			/* sqlStmt was already freed by bindRelativeKeyNamesToStatement() */
			/* bindRelativeKeyNamesToStatement() should've set an error in the parentKey */
			return -1;
		}

		/* Now our statement handle is ready to use */
		/* Execute the delete query (not persisted until transaction is committed!) */
		SQLLEN affectedRows = executeQuery (sqlStmt, queryDelete, parentKey);

		if (affectedRows == -1)
		{
			/* error, statement handle got freed by executeQuery() */
			return -1;
		}
		else if (affectedRows > 0)
		{
			sumAffectedRows += affectedRows;
		}
		else
		{
			sumAffectedRows = -2;
		}
	}

	elektraFree (queryDelete);

	return sumAffectedRows;
}


/**
 * @internal
 *
 * @brief This function is used to process the metadata associated with an modified Key.
 *
 * A Key is also considered as modified if only its metadata has changed, but not its value.
 * This function may execute INSERT-, UPDATE- and/or DELETE-queries that affect the metatable.
 * Which queries are executed depends on the changes of the metadata for the given Key.
 *
 * @param sqlStmt The statement on which the queries should be executed.
 * 	If the given statement already contains bound parameters, they are overwritten by this function.
 * @param modifiedKey The key for which the associated metadata should be processed.
 * @param dsConfig The data source configuration (contains mountpoint definition).
 * @param diffSet The ElektraDiff that was created by comparing the data to store with the data that is currently in the data source.
 * @param quoteStr The char(s) that should be used for quoting identifiers.
 * @param[in,out] parentKey The key-name of this key is needed to determine the relative key-name of the modified Key.
 * 	Used to store errors and warnings.
 *
 * @return The number of the deleted rows.
 * @retval -1 if an error occurred.
 * @retval -2 it the number of deleted rows could not be retrieved (e.g. because the used ODBC driver doesn't support this feature).
 */
static SQLLEN processMetaDataForModifiedKey (SQLHSTMT sqlStmt, Key * modifiedKey, const struct dataSourceConfig * dsConfig,
					     const ElektraDiff * diffSet, const char * quoteStr, Key * parentKey)
{
	/* For the UPDATE operation, we've to consider added, modified and deleted metadata
	 * --> this implies we may need INSERT, UPDATE and DELETE queries for the metadata */

	KeySet * ksMetaRemoved = elektraDiffGetRemovedMetaKeys (diffSet, modifiedKey);

	SQLLEN curAffectedRows;
	SQLLEN sumAffectedRows = 0;

	if (ksGetSize (ksMetaRemoved) > 0)
	{
		/* DELETE FROM metaTable where keyname=? and metaKeyName IN (?,?,?,...) */

		/* At first, we've to bind the key-name */
		if (!bindStringsToStatement (sqlStmt, 1, parentKey, 1, elektraKeyGetRelativeName (modifiedKey, parentKey)))
		{
			/* sqlStmt was already freed by the binding functions */
			ksDel (ksMetaRemoved);
			return -1;
		}

		char * queryMetaDelete = NULL;
		ssize_t numMetaRemoved = ksGetSize (ksMetaRemoved);
		ssize_t endPos = -1;

		for (ssize_t startPos = 0; numMetaRemoved > 0; startPos = endPos + 1)
		{
			/* Get the SQL DELETE query with the correct number of parameters for this iteration */
			if (numMetaRemoved > ODBC_SQL_MAX_PARAMS)
			{
				if (!queryMetaDelete)
				{
					queryMetaDelete = getDeleteMetaDataQuery (ODBC_SQL_MAX_PARAMS, dsConfig, quoteStr, parentKey);
				}

				endPos += ODBC_SQL_MAX_PARAMS;
				numMetaRemoved -= ODBC_SQL_MAX_PARAMS;
			}
			else
			{
				if (queryMetaDelete)
				{
					elektraFree (queryMetaDelete);
				}

				queryMetaDelete = getDeleteMetaDataQuery (numMetaRemoved, dsConfig, quoteStr, parentKey);
				endPos += numMetaRemoved;

				/* We are in the last iteration */
				numMetaRemoved = 0;
			}

			if (!queryMetaDelete)
			{
				ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
				SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
				ksDel (ksMetaRemoved);
				return -1;
			}

			/* Now, we bind the names of the metakeys */
			if (!bindKeyNamesToStatement (sqlStmt, ksMetaRemoved, 2, startPos, endPos, false, parentKey))
			{
				/* sqlStmt was already freed by the binding functions */
				ksDel (ksMetaRemoved);
				elektraFree (queryMetaDelete);
				return -1;
			}

			startPos = endPos + 1;
			curAffectedRows = executeQuery (sqlStmt, queryMetaDelete, parentKey);

			if (curAffectedRows == -1)
			{
				/* sqlStmt was already freed by executeQuery() */
				ksDel (ksMetaRemoved);
				return -1;
			}
			else if (curAffectedRows > 0)
			{
				sumAffectedRows += curAffectedRows;
			}
		}

		elektraFree (queryMetaDelete);
	}

	ksDel (ksMetaRemoved);


	KeySet * ksMetaAdded = elektraDiffGetAddedMetaKeys (diffSet, modifiedKey);

	if (ksGetSize (ksMetaAdded) > 0)
	{
		/* INSERT INTO metaTable (keyName, metaKeyName, metaValName) VALUES (?,?,?) */
		char * queryMetaInsert = getInsertQuery (dsConfig, true, quoteStr, parentKey);

		if (!queryMetaInsert)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			ksDel (ksMetaAdded);
			return -1;
		}

		for (elektraCursor itMeta = 0; itMeta < ksGetSize (ksMetaAdded); itMeta++)
		{
			Key * curMetaKey = ksAtCursor (ksMetaAdded, itMeta);

			if (!bindStringsToStatement (sqlStmt, 1, parentKey, 3, elektraKeyGetRelativeName (modifiedKey, parentKey),
						     keyBaseName (curMetaKey), keyString (curMetaKey)))
			{
				/* sqlStmt was already freed by the bindStringsToStatement() */
				ksDel (ksMetaAdded);
				elektraFree (queryMetaInsert);
				return -1;
			}

			curAffectedRows = executeQuery (sqlStmt, queryMetaInsert, parentKey);

			if (curAffectedRows == -1)
			{
				/* sqlStmt was already freed by executeQuery() */
				ksDel (ksMetaAdded);
				elektraFree (queryMetaInsert);
				return -1;
			}
			else if (curAffectedRows > 0)
			{
				sumAffectedRows += curAffectedRows;
			}
		}

		elektraFree (queryMetaInsert);
	}

	ksDel (ksMetaAdded);


	KeySet * ksMetaModified = elektraDiffGetModifiedMetaKeys (diffSet, modifiedKey);

	if (ksGetSize (ksMetaModified) > 0)
	{
		/* UPDATE metaTable SET metaval=? where keyName=? and metaKeyName=? */
		char * queryMetaUpdate = getUpdateQuery (dsConfig, true, quoteStr, parentKey);

		if (!queryMetaUpdate)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			ksDel (ksMetaModified);
			return -1;
		}

		KeySet * ksNewMeta = keyMeta (modifiedKey);
		for (elektraCursor itMeta = 0; itMeta < ksGetSize (ksMetaModified); itMeta++)
		{
			Key * curMetaKey = ksAtCursor (ksMetaModified, itMeta);

			/* The curMetaKey contains the old metavalue --> we have to get the metakey from the modified key */
			Key * newMetaKey = ksLookup (ksNewMeta, curMetaKey, KDB_O_NONE);


			if (!bindStringsToStatement (sqlStmt, 1, parentKey, 3, keyString (newMetaKey),
						     elektraKeyGetRelativeName (modifiedKey, parentKey), keyBaseName (newMetaKey)))
			{
				/* sqlStmt was already freed by the bindStringsToStatement() */
				ksDel (ksMetaModified);
				elektraFree (queryMetaUpdate);
				return -1;
			}

			curAffectedRows = executeQuery (sqlStmt, queryMetaUpdate, parentKey);

			if (curAffectedRows == -1)
			{
				/* sqlStmt was already freed by executeQuery() */
				ksDel (ksMetaModified);
				elektraFree (queryMetaUpdate);
				return -1;
			}
			else if (curAffectedRows > 0)
			{
				sumAffectedRows += curAffectedRows;
			}
		}

		elektraFree (queryMetaUpdate);
	}

	ksDel (ksMetaModified);

	return sumAffectedRows;
}


/**
 * @internal
 *
 * @brief This function is used for inserting or updating rows in the key-table and metatable based on the given KeySet and ElektraDiff
 *
 * @param sqlStmt The statement on which the queries should be executed.
 * 	If the given statement already contains bound parameters, they are overwritten by this function.
 * @param ks The KeySet that contains the Keys that should be inserted or updated.
 * 	The Keys inside @ks must contain the new-values which you want to store in the ODBC data source.
 * @param dsConfig The data source configuration (contains mountpoint definition).
 * @param update 'true' if existing rows should be updated, 'false' if new rows should be inserted.
 * @param diffSet The ElektraDiff that was created by comparing the data to store with the data that is currently in the data source.
	 @p diffSet is only needed for UPDATE operation, for INSERT it is ignored.
 * @param quoteStr The char(s) that should be used for quoting identifiers.
 * @param[in,out] parentKey The key-name of this key is needed to determine the relative key-name of the Keys in @ks.
 * 	Used to store errors and warnings.
 *
 * @return The number of the deleted rows.
 * @retval -1 if an error occurred.
 * @retval -2 it the number of deleted rows could not be retrieved (e.g. because the used ODBC driver doesn't support this feature).
 */
static SQLLEN insertOrUpdateRows (SQLHSTMT sqlStmt, const KeySet * ks, const struct dataSourceConfig * dsConfig, bool update,
				  const ElektraDiff * diffSet, const char * quoteStr, Key * parentKey)
{

	if (ksGetSize (ks) < 1)
	{
		/* no rows to insert */
		return 0;
	}

	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided statement handle must not be NULL.");
		return -1;
	}

	if (!dsConfig)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided dataSourceConfig struct must not be NULL.");
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return -1;
	}

	if (update && !diffSet)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "For the UPDATE operation, the diffSet must not be NULL.");
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return -1;
	}


	char * query;

	if (update)
	{
		query = getUpdateQuery (dsConfig, false, quoteStr, parentKey);
	}
	else
	{
		query = getInsertQuery (dsConfig, false, quoteStr, parentKey);
	}

	if (!query)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return -1;
	}

	SQLLEN curAffectedRows;

	/* The value -2 indicates that the number of affected rows could not be retrieved from the data source */
	SQLLEN sumAffectedRows = 0;

	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * curKey = ksAtCursor (ks, it);

		/* For the UPDATE operation, we have to check if the value was changed, it is possible that only metadata was changed */
		if (!update || elektraDiffKeyValueChanged (diffSet, curKey))
		{
			/* UPDATE table set val=? where key=?
			 * --> we have to bind the value first and then the key-name for which the value should be set
			 * The INSERT query was designed to have the value before the key-name, so that the same parameter order as in the
			 * UPDATE query can be used */
			if (!bindStringsToStatement (sqlStmt, 1, parentKey, 2, keyString (curKey),
						     elektraKeyGetRelativeName (curKey, parentKey)))
			{
				/* sqlStmt was already freed by bindStringsToStatement() */
				/* bindStringsToStatement() should've set an error in the parentKey */
				elektraFree (query);
				return -1;
			}

			/* Now our statement handle is ready to use for executing the query */
			curAffectedRows = executeQuery (sqlStmt, query, parentKey);

			if (curAffectedRows == -1)
			{
				/* sqlStmt was already freed by executeQuery() */
				elektraFree (query);
				return -1;
			}
			else if (curAffectedRows > 0)
			{
				sumAffectedRows += curAffectedRows;
			}
		}


		/* Now process the metadata, this KeySet MUST NOT be deleted */
		KeySet * curMetaKs = keyMeta (curKey);


		if (update)
		{
			curAffectedRows = processMetaDataForModifiedKey (sqlStmt, curKey, dsConfig, diffSet, quoteStr, parentKey);
			if (curAffectedRows == -1)
			{
				elektraFree (query);
			}
			else if (curAffectedRows > 0)
			{
				sumAffectedRows += curAffectedRows;
			}
		}
		else if (ksGetSize (curMetaKs) > 0)
		{
			char * queryMetaInsert = getInsertQuery (dsConfig, true, quoteStr, parentKey);

			for (elektraCursor itMeta = 0; itMeta < ksGetSize (curMetaKs); itMeta++)
			{
				Key * curMetaKey = ksAtCursor (curMetaKs, itMeta);

				/* INSERT metadata (3 parameters)
				INSERT INTO metaTableName (keyColName, metaKeyColName, valColName) VALUES (?,?,?) */
				if (!bindStringsToStatement (sqlStmt, 1, parentKey, 3, elektraKeyGetRelativeName (curKey, parentKey),
							     keyBaseName (curMetaKey), keyString (curMetaKey)))
				{
					elektraFree (query);
					elektraFree (queryMetaInsert);
					return -1;
				}

				/* Now insert the keys */
				curAffectedRows = executeQuery (sqlStmt, queryMetaInsert, parentKey);

				if (curAffectedRows == -1)
				{
					/* sqlStmt was already freed by executeQuery() */
					elektraFree (query);
					elektraFree (queryMetaInsert);
					return -1;
				}
				else if (curAffectedRows > 0)
				{
					sumAffectedRows += curAffectedRows;
				}
			}

			elektraFree (queryMetaInsert);
		}
	}

	elektraFree (query);
	return sumAffectedRows;
}


#ifdef DEBUG
#if DEBUG
/**
 * @internal
 *
 * @brief Print all keys that was detected as added, removed or modified.
 * 	Mainly useful for logging and debugging.
 *
 * @param diffSet The ElektraDiff that was created by comparing the data to store with the data that is currently in the data source.
 * @param parentKey The key-name of this key is used to print the relative key-names of the affected Keys.
 * 	This relative key-names are actually stored in the datasource and enables the support for mounting the data source
 * 	at different places in the KDB.
 */
static void logChangedKeys (ElektraDiff * diffSet, Key * parentKey)
{
	ELEKTRA_LOG ("Added keys:\n");
	KeySet * ksDiff = elektraDiffGetAddedKeys (diffSet);
	for (elektraCursor it = 0; it < ksGetSize (ksDiff); it++)
	{
		ELEKTRA_LOG ("keyname: %s, string: %s, relative: %s\n", keyName (ksAtCursor (ksDiff, it)), keyString (ksAtCursor (ksDiff, it)),
			     elektraKeyGetRelativeName (ksAtCursor (ksDiff, it), parentKey));
	}
	ksDel (ksDiff);

	ELEKTRA_LOG ("Removed keys:\n");
	ksDiff = elektraDiffGetRemovedKeys (diffSet);
	for (elektraCursor it = 0; it < ksGetSize (ksDiff); it++)
	{
		ELEKTRA_LOG ("keyname: %s, string: %s, relative: %s\n", keyName (ksAtCursor (ksDiff, it)), keyString (ksAtCursor (ksDiff, it)),
			     elektraKeyGetRelativeName (ksAtCursor (ksDiff, it), parentKey));
	}
	ksDel (ksDiff);

	ELEKTRA_LOG ("Modified keys:\n");
	ksDiff = elektraDiffGetModifiedKeys (diffSet);
	for (elektraCursor it = 0; it < ksGetSize (ksDiff); it++)
	{
		ELEKTRA_LOG ("keyname: %s, string: %s, relative: %s\n", keyName (ksAtCursor (ksDiff, it)), keyString (ksAtCursor (ksDiff, it)),
			     elektraKeyGetRelativeName (ksAtCursor (ksDiff, it), parentKey));
	}
	ksDel (ksDiff);
}
#endif
#endif

/**
 * @brief Change the data in the ODBC data source so that it represents the state the is given in @ks.
 * 	The means inserting, updating and deleting rows in the key-table and in the metatable.
 *
 * @pre This given @p sharedData struct MUST contain an allocated and valid dsConfig (struct dataSourceConfiguration)
 * AND it must contain NULL pointers for the environment and the connection handle.
 *
 * @post If the function returns successfully, the given @p sharedData contains a handle to a valid and open connection and to an
 * allocated ODBC environment.
 * This connection must be used to commit or rollback the transaction later.
 *
 * Please note that this functions starts a new transaction, but does NOT commit it.
 * If you want to finish the transaction (commit or rollback), you have to either call endTransaction() or the related functions
 * of the ODBC API directly.
 *
 * @param sharedData This struct is used to share data like the data source configurations, connections and ODBC environment handles
 * 	between different parts and phases of the ODBC backend plugin.
 *
 *
 * @param ks The KeySet that contains the state that should be persisted in the data source.
 * @param[in,out] parentKey The key-name of this key is needed to determine the relative key-name of the Keys in @ks.
 * 	Used to store errors and warnings.
 *
 * @return The number of the deleted rows.
 * @retval -1 if an error occurred.
 * @retval -2 it the number of deleted rows could not be retrieved (e.g. because the used ODBC driver doesn't support this feature).
 */
SQLLEN storeKeysInDataSource (struct odbcSharedData * sharedData, KeySet * ks, Key * parentKey)
{
	/* Arguments are checked inside getKeysFromDataSource() */

	/* 1. Get the current data from the data source and the string for quoting identifiers*/
	/* This starts a new transaction that is kept open. The handles for the ODBC environment and connection are stored in sharedData */
	KeySet * ksDs = getKeysFromDataSource (sharedData, true, parentKey);
	if (!ksDs)
	{	/* error should've been set be getKeysFromDataSource */
		return -1;
	}

	char * quoteStr = getQuoteStr (sharedData->connection, parentKey);

	if (!quoteStr || !(*quoteStr))
	{
		elektraFree (quoteStr);
		/* An error on the parent key should've been set by getQuoteStr() */
		ksDel (ksDs);
		return -1;
	}


	/* 2. Compare the data from the data source with the KeySet which should be persisted, create an ElektraDiff */
	ElektraDiff * diffSet = elektraDiffCalculate (ks, ksDs, parentKey);
	ksDel (ksDs);

	if (!diffSet || elektraDiffIsEmpty (diffSet))
	{
		/* no update */
		elektraFree (quoteStr);
		elektraDiffDel (diffSet);
		return 0;
	}

#if DEBUG
	logChangedKeys (diffSet, parentKey);
#endif
	/* 3. Execute the DELETE, UPDATE, and INSERT queries based on the calculated diff */
	SQLHSTMT sqlStmt = NULL;
	SQLLEN sumAffectedRows = 0;

	KeySet * ksRemovedKeys = elektraDiffGetRemovedKeys (diffSet);

	if (ksGetSize (ksRemovedKeys) > 0)
	{
		/* We can delete all keys with 2 queries (one for the metatable, one for the keytable)
		 * If foreign key constraints and cascading delete is available and configured in the data source, even one query is enough
		 * DELETE FROM table WHERE keyname IN (val1, val2, ...) */

		sqlStmt = allocateStatementHandle (sharedData->connection, parentKey);

		if (!sqlStmt)
		{
			elektraFree (quoteStr);
			ksDel (ksRemovedKeys);
			elektraDiffDel (diffSet);
			return -1;
		}

		/* TODO: add configuration option for data sources that have cascading delete enabled
		 * --> no extra deletion of tuples in the metatable required */

		/* At first delete the rows from the meta table */
		SQLLEN curAffectedRows = removeRows (sqlStmt, ksRemovedKeys, sharedData->dsConfig->metaTableName,
						     sharedData->dsConfig->metaTableKeyColName, quoteStr, parentKey);
		if (curAffectedRows == -1)
		{
			elektraFree (quoteStr);
			ksDel (ksRemovedKeys);
			elektraDiffDel (diffSet);
			return -1;
		}
		else if (curAffectedRows > 0)
		{
			sumAffectedRows += curAffectedRows;
		}

		/* We can use the already bound parameters for deleting rows from the 2nd table (key-table) */
		curAffectedRows = removeRows (sqlStmt, ksRemovedKeys, sharedData->dsConfig->tableName, sharedData->dsConfig->keyColName,
					      quoteStr, parentKey);
		ELEKTRA_LOG ("DELETE affected %ld rows!\n", curAffectedRows);

		if (curAffectedRows == -1)
		{
			elektraFree (quoteStr);
			ksDel (ksRemovedKeys);
			elektraDiffDel (diffSet);
			return -1;
		}
		else if (curAffectedRows > 0)
		{
			sumAffectedRows += curAffectedRows;
		}
	}

	ksDel (ksRemovedKeys);

	/* The keys in ksModifiedKeys contain the old values, but we need the new values for storing the in the data source */
	KeySet * ksModifiedNewKeys = elektraDiffGetModifiedNewKeys (diffSet);

	if (ksGetSize (ksModifiedNewKeys) > 0)
	{
		if (!ksModifiedNewKeys)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
			elektraFree (quoteStr);
			elektraDiffDel (diffSet);
			return -1;
		}

		if (!sqlStmt)
		{
			sqlStmt = allocateStatementHandle (sharedData->connection, parentKey);

			if (!sqlStmt)
			{
				elektraFree (quoteStr);
				ksDel (ksModifiedNewKeys);
				elektraDiffDel (diffSet);
				return -1;
			}
		}

		SQLLEN curAffectedRows =
			insertOrUpdateRows (sqlStmt, ksModifiedNewKeys, sharedData->dsConfig, true, diffSet, quoteStr, parentKey);


		ELEKTRA_LOG ("UPDATE affected %ld rows!\n", curAffectedRows);

		if (curAffectedRows == -1)
		{
			elektraFree (quoteStr);
			ksDel (ksModifiedNewKeys);
			elektraDiffDel (diffSet);
			return -1;
		}
		else if (curAffectedRows > 0)
		{
			sumAffectedRows += curAffectedRows;
		}
	}

	ksDel (ksModifiedNewKeys);


	KeySet * ksAddedKeys = elektraDiffGetAddedKeys (diffSet);

	if (ksGetSize (ksAddedKeys) > 0)
	{
		if (!sqlStmt)
		{
			sqlStmt = allocateStatementHandle (sharedData->connection, parentKey);

			if (!sqlStmt)
			{
				elektraFree (quoteStr);
				ksDel (ksAddedKeys);
				elektraDiffDel (diffSet);
				return -1;
			}
		}

		SQLLEN curAffectedRows = insertOrUpdateRows (sqlStmt, ksAddedKeys, sharedData->dsConfig, false, NULL, quoteStr, parentKey);

		ELEKTRA_LOG ("INSERT affected %ld rows!\n", curAffectedRows);

		if (curAffectedRows == -1)
		{
			elektraFree (quoteStr);
			ksDel (ksAddedKeys);
			elektraDiffDel (diffSet);
			return -1;
		}
		else if (curAffectedRows > 0)
		{
			sumAffectedRows += curAffectedRows;
		}
	}

	ksDel (ksAddedKeys);

	elektraFree (quoteStr);
	elektraDiffDel (diffSet);

	/* No disconnecting or freeing of handles here, the handle to the connection and the SQL environment are needed until
	 * the transaction is committed or rolled back. */
	ELEKTRA_LOG_DEBUG ("Finished executing queries in open transaction, make sure to end the transaction.");
	return sumAffectedRows;
}

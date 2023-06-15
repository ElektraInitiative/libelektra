/**
 * @file
 *
 * @brief Source file for general functions needed by the ODBC backend plugin.
 *
 * This is the place for functions that are needed for setting up the environment or are used by both, get- and set-operations
 * Some examples are allocating various handles, setting general attributes, connecting the data sources, etc.
 * This file is intended esp. for ODBC related tasks.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include "./backend_odbc_general.h"

#include <kdberrors.h>
#include <kdblogger.h>
#include <sqlext.h>
#include <string.h>


/**
 * @brief Allocate a new ODBC environment handle
 *
 * @param[out] errorKey Used to store errors and warnings
 *
 * @return The allocated environment handle
 * 	Make sure to free the returned handle by calling SQLFreeHandle().
 * @retval NULL if an error occurred (see @p errorKey for details)
 */
SQLHENV allocateEnvHandle (Key * errorKey)
{
	SQLHENV sqlEnv = NULL;
	SQLRETURN ret = SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &sqlEnv);

	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate handle for ODBC environment!");
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		return NULL;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_ENV, sqlEnv, errorKey);
	}

	return sqlEnv;
}

/**
 * @brief Allocate a new ODBC connection handle
 *
 * @param sqlEnv An allocated ODBC environment handle, for which the ODBC version has been set
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned NULL.
 * @param[out] errorKey Used to store errors and warnings
 *
 * @return The allocated connection handle
 * 	Make sure to free the returned handle by calling SQLFreeHandle().
 * @retval NULL on errors (see @p errorKey for details)
 *
 * @see allocateEnvHandle() for getting a new environment handle
 * @see setOdbcVersion() for setting the ODBC version that should be used with the environment the handle refers to
 */
SQLHDBC allocateConnectionHandle (SQLHENV sqlEnv, Key * errorKey)
{
	SQLHDBC sqlConnection = NULL;
	SQLRETURN ret = SQLAllocHandle (SQL_HANDLE_DBC, sqlEnv, &sqlConnection);

	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle () failed!\nCould not allocate handle for ODBC connection!");
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_ENV, sqlEnv, errorKey);

		if (sqlConnection)
		{
			SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		}

		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_ENV, sqlEnv, errorKey);
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_DBC, sqlConnection, errorKey);
	}

	return sqlConnection;
}


/**
 * @brief Allocate a new ODBC statement handle
 *
 * @param sqlConnection An initialized connection handle. It must represent an active connection.
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned NULL.
 * @param[out] errorKey Used to store errors and warnings
 *
 * @return The allocated statement handle
 * 	Make sure to free the returned handle by calling SQLFreeHandle().
 * @retval NULL on errors (see @p errorKey for details)
 *
 * @see allocateEnvHandle() for getting a new environment handle
 * @see allocateConnectionHandle() for getting a new connection handle
 * @see connectToDataSource() for setting up an active connection, this is needed for allocating a statement with this function
 */
SQLHDBC allocateStatementHandle (SQLHDBC sqlConnection, Key * errorKey)
{
	/* Handle for a statement */
	SQLHSTMT sqlStmt = NULL;
	SQLRETURN ret = SQLAllocHandle (SQL_HANDLE_STMT, sqlConnection, &sqlStmt);

	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate a handle for an SQL statement!");
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_DBC, sqlConnection, errorKey);

		if (sqlStmt)
		{
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		}

		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return NULL;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_DBC, sqlConnection, errorKey);
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, sqlStmt, errorKey);
	}

	return sqlStmt;
}


/**
 * @brief Set the ODBC version of an allocated ODBC environment handle
 *
 * This is required by ODBC before allocation a connection handle.
 *
 * @param sqlEnv An allocated ODBC environment handle, as returned by allocateEnvHandle()
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned 'false'.
 * @param[out] errorKey Used to store errors and warnings
 *
 * @retval 'true' if the operation was successful
 * @retval 'false' if an error occurred (see @p errorKey for details)
 *
 * @see allocateEnvHandle() for getting an environment handle that can be passed to this function
 * @see https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/declaring-the-application-s-odbc-version for more information.
 */
bool setOdbcVersion (SQLHENV sqlEnv, unsigned long version, Key * errorKey)
{
	SQLRETURN ret = SQLSetEnvAttr (sqlEnv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) version, 0);

	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLSetEnvAttr() failed!\nCould not set ODBC-version attribute for ODBC environment!");
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_ENV, sqlEnv, errorKey);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_ENV, sqlEnv, errorKey);
	}

	return true;
}


/**
 * @brief Sets the timeout after which a login attempt (connect to data source) is considered as failed
 *
 * @param sqlConnection An allocated ODBC connection handle
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned 'false'.
 * @param timeout The time to wait in seconds (0 means waiting indefinitely, use with care!)
 * @param[out] errorKey Used to store errors and warnings
 *
 * @retval 'true' if the operation was successful
 * @retval 'false' if an error occurred (see @p errorKey for details)
 *
 * @see allocateConnectionHandle() for getting a connection handle that can be passed to this function
 */
bool setLoginTimeout (SQLHDBC sqlConnection, uintptr_t timeout, Key * errorKey)
{
	SQLRETURN ret = SQLSetConnectAttr (sqlConnection, SQL_LOGIN_TIMEOUT, (SQLPOINTER) timeout, 0);

	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLSetConnectAttr() failed!\nCould not set timeout attribute for ODBC connection!");
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_DBC, sqlConnection, errorKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);

		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_DBC, sqlConnection, errorKey);
	}

	return true;
}


/**
 * @brief Sets the transaction mode (automatic or manual commits) for an ODBC connection
 *
 * @param sqlConnection An allocated ODBC connection handle
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned 'false'.
 * @param enableAutoCommit Should every SQL query automatically commit a transaction (true)
 * 	or should manual transaction control be used (false)
 * @param[out] errorKey Used to store errors and warnings
 *
 * @retval 'true' if the operation was successful
 * @retval 'false' if an error occurred (see @p errorKey for details)
 *
 * @see allocateConnectionHandle() for getting a connection handle that can be passed to this function
 * @see https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/transactions-odbc for more information about transactions in ODBC
 */
bool setAutocommit (SQLHDBC sqlConnection, bool enableAutoCommit, Key * errorKey)
{
	SQLRETURN ret;
	if (enableAutoCommit)
	{
		ret = SQLSetConnectAttr (sqlConnection, SQL_ATTR_AUTOCOMMIT, (SQLPOINTER) SQL_AUTOCOMMIT_ON, 0);
	}
	else
	{
		/* set manual transaction mode (disable autocommit) */
		ret = SQLSetConnectAttr (sqlConnection, SQL_ATTR_AUTOCOMMIT, (SQLPOINTER) SQL_AUTOCOMMIT_OFF, 0);
	}

	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLSetConnectAttr () failed!\nCould not set the autocommit mode for an ODBC connection!");
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_DBC, sqlConnection, errorKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_DBC, sqlConnection, errorKey);
	}

	return true;
}


/**
 * @brief Ends (commit or rollback) the currently active transaction on the given connection
 *
 * @pre The connection handle must refer to an open connection, for which manual transaction mode is active and a transaction was started
 *
 * @param sqlConnection An ODBC connection handle that satisfies the mentioned preconditions
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned 'false'.
 * @param commit 'true' if the transaction should be committed,
 * 	'false' if the transaction should be rolled back (canceled)
 * @param[out] errorKey Used to store errors and warnings
 *
 * @retval 'true' if the operation was successful
 * @retval 'false' if an error occurred (see @p errorKey for details)
 *
 * @see setAutocommit() for setting the transaction mode to manual commits
 * @see https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/transactions-odbc for more information about transactions in ODBC
 */
bool endTransaction (SQLHDBC sqlConnection, bool commit, Key * errorKey)
{
	SQLRETURN ret;
	if (commit)
	{
		ret = SQLEndTran (SQL_HANDLE_DBC, sqlConnection, SQL_COMMIT);
	}
	else
	{
		ret = SQLEndTran (SQL_HANDLE_DBC, sqlConnection, SQL_ROLLBACK);
	}

	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLEndTran () failed!\nCould not %s the transaction!", (commit ? "commit" : "rollback"));
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_DBC, sqlConnection, errorKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_DBC, sqlConnection, errorKey);
	}

	return true;
}


/**
 * @brief Establish a connection to an ODBC data source.
 *
 * @param sqlConnection An allocated ODBC connection handle.
 * 	This handle gets freed if an error occurred, so don't dereference it if the function returned 'false'.
 * @param dsConfig A valid data source configuration.
 * @param[out] errorKey Used to store errors and warnings.
 *
 * @retval 'true' if the operation was successful.
 * @retval 'false' if an error occurred (see @p errorKey for details).
 *
 * @see allocateConnectionHandle() for getting a connection handle that can be passed to this function.
 * @see fillDsStructFromDefinitionKs() for getting a @p dsConfig that can be passed to this function.
 */
bool connectToDataSource (SQLHDBC sqlConnection, const struct dataSourceConfig * dsConfig, Key * errorKey)
{
	if (!dsConfig || !(dsConfig->dataSourceName))
	{
		return NULL;
	}

	SQLRETURN ret = SQLConnect (sqlConnection, (SQLCHAR *) dsConfig->dataSourceName, SQL_NTS,
				    (SQLCHAR *) (dsConfig->userName ? dsConfig->userName : ""), SQL_NTS,
				    (SQLCHAR *) (dsConfig->password ? dsConfig->password : ""), SQL_NTS);

	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE (
			"SQLConnect() failed!\nCould not connect to the ODBC data source %s as user %s! Maybe the data source name, "
			"username or password is wrong!",
			dsConfig->dataSourceName, dsConfig->userName);
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_DBC, sqlConnection, errorKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_DBC, sqlConnection, errorKey);
	}

	ELEKTRA_LOG_DEBUG ("Connected to the ODBC data source '%s' as user '%s'.", dsConfig->dataSourceName, dsConfig->userName);
	return true;
}

/**
 * @brief Execute an SQL query on a given statement handle.
 *
 * @pre The statement must have bounded the correct values for each of the parameter markers ('?') in the given query string.
 *
 * @param stmt The statement on which the query should be executed.
 * @param query The query to execute.
 * @param[out] errorKey Use to store errors and warnings.
 *
 * @return The number of affected rows as reported by the ODBC driver.
 * @retval -1 on error.
 * @retval -2 if the number of affected rows was not reported by the ODBC driver.
 */
SQLLEN executeQuery (SQLHSTMT stmt, const char * query, Key * errorKey)
{
	if (!stmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (errorKey, "The provided statement handle must not be null.");
	}

	SQLRETURN ret = SQLExecDirect (stmt, (SQLCHAR *) query, SQL_NTS);

	if (SQL_SUCCEEDED (ret) || ret == SQL_NO_DATA)
	{
		if (ret == SQL_SUCCESS_WITH_INFO)
		{
			ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, stmt, errorKey);
		}
	}
	else
	{
		ELEKTRA_SET_ODBC_ERROR (SQL_HANDLE_STMT, stmt, errorKey);
		SQLFreeHandle (SQL_HANDLE_STMT, stmt);
		return -1;
	}


	SQLLEN rowCount = 0;
	ret = SQLRowCount (stmt, &rowCount);

	if (SQL_SUCCEEDED (ret))
	{
		if (ret == SQL_SUCCESS_WITH_INFO)
		{
			ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_STMT, stmt, errorKey);
		}

		return rowCount;
	}
	else
	{
		ELEKTRA_ADD_INTERNAL_WARNINGF (errorKey, "Could not get the number of affected rows for the following SQL query: %s",
					       query);
		return -2;
	}
}

/**
 * Get the character or string that should be used for quoting identifiers.
 * The value is requested from the ODBC driver.
 *
 * If no char or string coule be retrieved, the standard quote character (string with one char) '"' (double quote) is returned.
 *
 * @param sqlConnection An open connection to an ODBC data source.
 * @param[out] errorKey Used to store errors and warnings.
 *
 * @return The string (probably only with one char + \0) that should be added before and after identifiers to quote them.
 * @retval "\"" If no quote string could be retrieved from the ODBC driver.
 * @retval NULL on error
 */
char * getQuoteStr (SQLHDBC sqlConnection, Key * errorKey)
{
	/* Get driver specific identifier quote character
	 * (see: https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/quoted-identifiers) for more information */

	char identifierQuoteChar[2] = { 0, 0 };
	SQLSMALLINT quoteCharLen = 0;
	SQLRETURN ret = SQLGetInfo (sqlConnection, SQL_IDENTIFIER_QUOTE_CHAR, identifierQuoteChar, 2, &quoteCharLen);

	/* Treat error as warning and try to use the standard value " for quoting identifiers */
	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_ADD_INTERNAL_WARNING (
			errorKey, "Could not get an identifier quote char from the driver, using \" as defined by the SQL-92 standard");
		identifierQuoteChar[0] = '"';
		identifierQuoteChar[1] = '\0';
	}

	if (!SQL_SUCCEEDED (ret) || ret == SQL_SUCCESS_WITH_INFO)
	{
		ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_DBC, sqlConnection, errorKey);
	}

	char * identifierQuoteStr = elektraMalloc ((quoteCharLen + 1) * sizeof (char));

	if (!identifierQuoteStr)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		return NULL;
	}

	if (quoteCharLen > 1)
	{
		/* TODO: Check support for Unicode */
		ELEKTRA_ADD_INTERNAL_WARNING (
			errorKey,
			"Got a string for the info SQL_IDENTIFIER_QUOTE_CHAR with more than one byte, this is unusual."
			"If you are using Unicode on MS Windows (UTF-16), please be aware that this could lead to errors.");

		ret = SQLGetInfo (sqlConnection, SQL_IDENTIFIER_QUOTE_CHAR, identifierQuoteStr, (SQLSMALLINT) (quoteCharLen + 1), NULL);

		if (!SQL_SUCCEEDED (ret))
		{
			/* Treat error as warning and try to use the standard value " for quoting identifiers */
			ELEKTRA_ADD_INTERNAL_WARNING (
				errorKey,
				"Could not get an identifier quote string from the driver, despite the driver states that the string for "
				"the info SQL_IDENTIFIER_QUOTE_CHAR has more than one character!");
			ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_DBC, sqlConnection, errorKey);
			strcpy (identifierQuoteStr, "\"");
		}
		else if (ret == SQL_SUCCESS_WITH_INFO)
		{
			ELEKTRA_ADD_ODBC_WARNING (SQL_HANDLE_DBC, sqlConnection, errorKey);
		}
	}
	else
	{
		strcpy (identifierQuoteStr, identifierQuoteChar);
	}

	return identifierQuoteStr;
}

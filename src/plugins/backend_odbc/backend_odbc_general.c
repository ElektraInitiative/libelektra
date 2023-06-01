/**
 * @file
 *
 * @brief Source file for general functions needed by the ODBC backend plugin.
 *
 * This is the place for functions that are needed for setting up the environment or are used by both, get- and set-operations
 * Some examples are allocating various handles, setting general attributes, connecting the data sources, etc.
 * This file is intended esp. for ODBC related tasks.
 * General helper functions should be place in backend_odbc_helpers
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */


#include "./backend_odbc_general.h"
#include <kdblogger.h>
#include <sqlext.h>
#include <stddef.h> /* for NULL */


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
		return NULL;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_ENV, sqlEnv, "allocateEnvHandle", true, errorKey);
	}

	return sqlEnv;
}


/**
 * @brief Set the ODBC version of an allocated ODBC environment handle
 *
 * This is required by ODBC before allocation a connection handle.
 *
 * @param sqlEnv An allocated ODBC environment handle, as returned by @ref allocateEnvHandle()
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
		setOdbcError (SQL_HANDLE_ENV, sqlEnv, "setOdbcVersion", false, errorKey);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_ENV, sqlEnv, "setOdbcVersion", true, errorKey);
	}

	return true;
}


/**
 * @brief Allocate a new ODBC connection handle
 *
 * @param sqlEnv An allocated ODBC environment handle, for which the ODBC version has been set
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
		setOdbcError (SQL_HANDLE_ENV, sqlEnv, "allocateConnectionHandle", false, errorKey);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_ENV, sqlEnv, "allocateConnectionHandle (environment)", true, errorKey);
		setOdbcError (SQL_HANDLE_DBC, sqlConnection, "allocateConnectionHandle (connection)", true, errorKey);
	}

	return sqlConnection;
}


/**
 * @brief Sets the timeout after a login attempt (= connect to data source) is considered as failed
 *
 * @param sqlConnection An allocated ODBC connection handle
 * @param timeout The time to wait in seconds (0 = wait indefinitely --> use with care!)
 * @param[out] errorKey Used to store errors and warnings
 *
 * @retval 'true' if the operation was successful
 * @retval 'false' if an error occurred (see @p errorKey for details)
 *
 * @see allocateConnectionHandle() for getting a connection handle that can be passed to this function
 */
bool setLoginTimeout (SQLHDBC sqlConnection, unsigned long timeout, Key * errorKey)
{
	SQLRETURN ret = SQLSetConnectAttr (sqlConnection, SQL_LOGIN_TIMEOUT, (SQLPOINTER) timeout, 0);

	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLSetConnectAttr() failed!\nCould not set timeout attribute for ODBC connection!");
		setOdbcError (SQL_HANDLE_DBC, sqlConnection, "setLoginTimeout", false, errorKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);

		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_DBC, sqlConnection, "setLoginTimeout", true, errorKey);
	}

	return true;
}


/**
 * @brief Sets the transaction mode (automatic or manual commits) for an ODBC connection
 *
 * @param sqlConnection An allocated ODBC connection handle
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
		setOdbcError (SQL_HANDLE_DBC, sqlConnection, "setAutocommit", false, errorKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_DBC, sqlConnection, "setAutocommit", true, errorKey);
	}

	return true;
}


/**
 * @brief Ends (commit or rollback) the currently active transaction on the given connection
 *
 * @pre The connection handle must refer to an open connection, for which manual transaction mode is active and a transaction was started
 *
 * @param sqlConnection An ODBC connection handle that satisfies the mentioned preconditions
 * @param commit 'true' if the transaction should be committed,
 * 	'false' if the transaction should be rolled back (= canceled)
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
		setOdbcError (SQL_HANDLE_DBC, sqlConnection, "endTransaction", false, errorKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_DBC, sqlConnection, "endTransaction", true, errorKey);
	}

	return true;
}


/**
 * @brief Establish a connection to an ODBC data source
 *
 * @param sqlConnection An allocated ODBC connection handle
 * @param dsConfig A valid data source configuration
 * @param[out] errorKey Used to store errors and warnings
 *
 * @retval 'true' if the operation was successful
 * @retval 'false' if an error occurred (see @p errorKey for details)
 *
 * @see allocateConnectionHandle() for getting a connection handle that can be passed to this function
 * @see fillDsStructFromDefinitionKs() for getting a @p dsConfig that can be passed to this function
 */
bool connectToDataSource (SQLHDBC sqlConnection, struct dataSourceConfig * dsConfig, Key * errorKey)
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
		setOdbcError (SQL_HANDLE_DBC, sqlConnection, "connectToDataSource", false, errorKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_DBC, sqlConnection, "connectToDataSource", true, errorKey);
	}

	ELEKTRA_LOG_DEBUG ("Connected to the ODBC data source '%s' as user '%s'.", dsConfig->dataSourceName, dsConfig->userName);
	return true;
}


/**
 * @brief Binds the buffers in the struct @p columnData to the columns defined by the @p sqlStmt
 *
 * This function assumes that there are four columns ('key-name', 'key-value', 'metakey-name' and 'metakey-value')
 *
 * @param sqlStmt The allocated and prepared SQL statement for which the columns should be bound
 * @param buffers The buffers where the data for that columns should be stored
 * @param[out] errorKey Used to store errors and warnings
 *
 * @retval 'true' if the operation was successful
 * @retval 'false' if an error occurred (see @p errorKey for details)
 *
 * @see prepareSelectStmt() for getting a statement handle that can be used by this function
 */
bool bindColumns (SQLHSTMT sqlStmt, struct columnData * buffers, Key * errorKey)
{
	/* TODO: Support for binary keys */

	/* Bind column 1 (key-name) */
	SQLRETURN ret = SQLBindCol (sqlStmt, 1, SQL_C_CHAR, buffers->bufferKeyName, KEYNAME_BUFFER_SIZE, &(buffers->nameLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the first column to the keyName!");
		setOdbcError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (1)", false, errorKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (1)", true, errorKey);
	}

	/* Bind column 2 (key-value) */
	ret = SQLBindCol (sqlStmt, 2, SQL_C_CHAR, buffers->bufferKeyStr, KEYSTRING_BUFFER_SIZE, &(buffers->strLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		setOdbcError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (2)", false, errorKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (2)", true, errorKey);
	}

	/* Bind column 3 (metakey-name) */
	ret = SQLBindCol (sqlStmt, 3, SQL_C_CHAR, buffers->bufferMetaKeyName, METAKEYNAME_BUFFER_SIZE, &(buffers->metaNameLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		setOdbcError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (3)", false, errorKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (3)", true, errorKey);
	}

	/* Bind column 4 (metakey-value) */
	ret = SQLBindCol (sqlStmt, 4, SQL_C_CHAR, buffers->bufferMetaKeyStr, METASTRING_BUFFER_SIZE, &(buffers->metaStrLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		setOdbcError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (4)", false, errorKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		setOdbcError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (4)", true, errorKey);
	}

	return true;
}
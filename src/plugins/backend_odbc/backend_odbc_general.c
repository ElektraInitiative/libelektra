#include "./backend_odbc_general.h"
#include <kdblogger.h>
#include <sqlext.h>
#include <stddef.h> /* for NULL */

SQLHENV allocateEnvHandle (Key * parentKey)
{
	SQLHENV sqlEnv = NULL;
	SQLRETURN ret = SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &sqlEnv);

	if (!SQL_SUCCEEDED (ret))
	{
		// fprintf (stderr, "Error AllocHandle\n");
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate handle for ODBC environment!");
		return NULL;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_ENV, sqlEnv, "allocateEnvHandle", true, parentKey);
	}

	return sqlEnv;
}


bool setOdbcVersion (SQLHENV sqlEnv, unsigned long version, Key * parentKey)
{
	SQLRETURN ret = SQLSetEnvAttr (sqlEnv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) version, 0);

	if (!SQL_SUCCEEDED (ret))
	{
		// fprintf (stderr, "Error SetEnvAttr\n");
		ELEKTRA_LOG_NOTICE ("SQLSetEnvAttr() failed!\nCould not set ODBC-version attribute for ODBC environment!");
		logError (SQL_HANDLE_ENV, sqlEnv, "setOdbcVersion", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_ENV, sqlEnv, "setOdbcVersion", true, parentKey);
	}

	return true;
}


SQLHDBC allocateConnectionHandle (SQLHENV sqlEnv, Key * parentKey)
{
	SQLHDBC sqlConnection = NULL;
	SQLRETURN ret = SQLAllocHandle (SQL_HANDLE_DBC, sqlEnv, &sqlConnection);

	if (!SQL_SUCCEEDED (ret))
	{
		// fprintf (stderr, "Error AllocDBC %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle () failed!\nCould not allocate handle for ODBC connection!");
		logError (SQL_HANDLE_ENV, sqlEnv, "allocateConnectionHandle", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_ENV, sqlEnv, "allocateConnectionHandle (environment)", true, parentKey);
		logError (SQL_HANDLE_DBC, sqlConnection, "allocateConnectionHandle (connection)", true, parentKey);
	}

	return sqlConnection;
}


bool setLoginTimeout (SQLHDBC sqlConnection, unsigned long timeout, Key * parentKey)
{
	SQLRETURN ret = SQLSetConnectAttr (sqlConnection, SQL_LOGIN_TIMEOUT, (SQLPOINTER) timeout, 0);

	if (!SQL_SUCCEEDED (ret))
	{
		// fprintf (stderr, "Error AllocHDB %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLSetConnectAttr() failed!\nCould not set timeout attribute for ODBC connection!");
		logError (SQL_HANDLE_DBC, sqlConnection, "setLoginTimeout", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);

		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_DBC, sqlConnection, "setLoginTimeout", true, parentKey);
	}

	return true;
}


bool setAutocommit (SQLHDBC sqlConnection, bool enableAutoCommit, Key * parentKey)
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
		// fprintf (stderr, "Error setAutocommit %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLSetConnectAttr () failed!\nCould not set the autocommit mode for an ODBC connection!");
		logError (SQL_HANDLE_DBC, sqlConnection, "setAutocommit", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_DBC, sqlConnection, "setAutocommit", true, parentKey);
	}

	return true;
}


bool endTransaction (SQLHDBC sqlConnection, bool commit, Key * parentKey)
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
		// fprintf (stderr, "Error endTransaction %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLEndTran () failed!\nCould not %s the transaction!", (commit ? "commit" : "rollback"));
		logError (SQL_HANDLE_DBC, sqlConnection, "endTransaction", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_DBC, sqlConnection, "endTransaction", true, parentKey);
	}

	return true;
}


bool connectToDataSource (SQLHDBC sqlConnection, struct dataSourceConfig * dsConfig, Key * parentKey)
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
		// fprintf (stderr, "Error SqlConnect %d\n", ret);
		ELEKTRA_LOG_NOTICE (
			"SQLConnect() failed!\nCould not connect to the ODBC data source %s as user %s! Maybe the data source name, "
			"username or password is wrong!",
			dsConfig.dataSourceName, dsConfig.userName);
		logError (SQL_HANDLE_DBC, sqlConnection, "connectToDataSource", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_DBC, sqlConnection, "connectToDataSource", true, parentKey);
	}

	ELEKTRA_LOG_DEBUG ("Connected to the ODBC data source '%s' as user '%s'.", dsConfig.dataSourceName, dsConfig.userName);
	return true;
}


bool bindColumns (SQLHSTMT sqlStmt, struct columnData * buffers, Key * parentKey)
{
	/* TODO: Support for binary keys */

	/* Bind column 1 (key-name) */
	SQLRETURN ret = SQLBindCol (sqlStmt, 1, SQL_C_CHAR, buffers->bufferKeyName, KEYNAME_BUFFER_SIZE, &(buffers->nameLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		// fprintf (stderr, "Error SqlBindCol1 (keyname) %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the first column to the keyName!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (1)", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (1)", true, parentKey);
	}

	/* Bind column 2 (key-value) */
	ret = SQLBindCol (sqlStmt, 2, SQL_C_CHAR, buffers->bufferKeyStr, KEYSTRING_BUFFER_SIZE, &(buffers->strLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		// fprintf (stderr, "Error SqlBindCol2 (keystring) %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (2)", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (2)", true, parentKey);
	}

	/* Bind column 3 (metakey-name) */
	ret = SQLBindCol (sqlStmt, 3, SQL_C_CHAR, buffers->bufferMetaKeyName, METAKEYNAME_BUFFER_SIZE, &(buffers->metaNameLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		// fprintf (stderr, "Error SqlBindCol3 (metaname) %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (3)", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (3)", true, parentKey);
	}

	/* Bind column 4 (metakey-value) */
	ret = SQLBindCol (sqlStmt, 4, SQL_C_CHAR, buffers->bufferMetaKeyStr, METASTRING_BUFFER_SIZE, &(buffers->metaStrLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		// fprintf (stderr, "Error SqlBindCol4 (metastring) %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (4)", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "bindColumns (4)", true, parentKey);
	}

	return true;
}



#include "../backend/backend.h"
#include "backendprivateOdbc.h"

#include <kdberrors.h>
#include <kdblogger.h>
#include <kdbprivate.h>

/* FIXME: Remove, only for testing during dev */
#include <stdio.h>

struct dataSourceConfig
{
	const char * dataSourceName;
	const char * userName;
	const char * password;
	const char * tableName;
	const char * keyColName;
	const char * valColName;
};


void logError (SQLSMALLINT handleType, SQLHANDLE handle, char * functionName, bool isInfo)
{
	/* Get number of available records */
	SQLINTEGER numRecs = 0;
	SQLGetDiagField (handleType, handle, 0, SQL_DIAG_NUMBER, &numRecs, 0, 0);


	/* Get the status records */
	for (SQLINTEGER i = 1; i <= numRecs; i++)
	{
		SQLCHAR sqlState[SQL_SQLSTATE_SIZE + 1];
		SQLCHAR errMsg[SQL_MAX_MESSAGE_LENGTH + 1];
		SQLINTEGER nativeError;
		SQLSMALLINT errMsgLen;


		SQLRETURN ret = SQLGetDiagRec (handleType, handle, i, sqlState, &nativeError, errMsg, SQL_MAX_MESSAGE_LENGTH, &errMsgLen);
		if (ret == SQL_NO_DATA)
		{
			break;
		}

		/* The official documentation from Microsoft states that "There is no maximum length of the diagnostic message text".
		 * Therefore, we check the actual length despite using the SQL_MAX_MESSAGE_LENGTH constant.
		 * see: https://learn.microsoft.com/en-us/sql/odbc/reference/syntax/sqlgetdiagrec-function (Arguments->BufferLength)
		 */
		SQLCHAR * longErrMsg;

		if (ret == SQL_SUCCESS_WITH_INFO && errMsgLen > SQL_MAX_MESSAGE_LENGTH)
		{
			longErrMsg = (SQLCHAR *) elektraMalloc (sizeof (SQLCHAR) * (errMsgLen + 1));

			if (longErrMsg)
			{
				SQLGetDiagRec (handleType, handle, i, sqlState, &nativeError, longErrMsg, errMsgLen, 0);
			}
		}
		else
		{
			longErrMsg = NULL;
		}

		fprintf (stderr, "An ODBC %s occurred", isInfo ? "warning" : "error");
		(functionName && *functionName) ? fprintf (stderr, " in the following function: %s\n", functionName) : fprintf (stderr, ":\n");
		fprintf (stderr, "Number of error or warning: %d of %d\n", i, numRecs);
		fprintf (stderr, "Status Code: %s\n", sqlState);
		fprintf (stderr, "Native error code: %d\n", nativeError);
		fprintf (stderr, "Error message: %s\n\n", longErrMsg ? longErrMsg : errMsg);
		elektraFree (longErrMsg);

	}
}


SQLHENV allocateEnvHandle (Key * parentKey)
{
	SQLHENV sqlEnv = NULL;
	SQLRETURN ret = SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &sqlEnv);


	if (!SQL_SUCCEEDED (ret))
	{
		/* TODO: Extract error and add it to key */
		fprintf (stderr, "Error AllocHandle\n");
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate handle for ODBC environment!");
		return NULL;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_ENV, sqlEnv, "allocateEnvHandle", true);
	}


	return sqlEnv;

}


bool setOdbcVersion (SQLHENV sqlEnv, unsigned long version, Key * parentKey)
{
	SQLRETURN ret = SQLSetEnvAttr (sqlEnv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) version, 0);

	if (!SQL_SUCCEEDED (ret))
	{
		/* TODO: Extract error and add it to key */
		fprintf (stderr, "Error SetEnvAttr\n");
		ELEKTRA_LOG_NOTICE ("SQLSetEnvAttr() failed!\nCould not set ODBC-version attribute for ODBC environment!");
		logError (SQL_HANDLE_ENV, sqlEnv, "setOdbcVersion", false);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_ENV, sqlEnv, "setOdbcVersion", true);
	}

	return true;
}


SQLHDBC allocateConnectionHandle (SQLHENV sqlEnv, Key * parentKey)
{
	SQLHDBC sqlConnection = NULL;
	SQLRETURN ret = SQLAllocHandle (SQL_HANDLE_DBC, sqlEnv, &sqlConnection);

	if (!SQL_SUCCEEDED (ret))
	{
		/* TODO: Extract error and add it to key */
		fprintf (stderr, "Error AllocHDB %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle () failed!\nCould not allocate handle for ODBC connection! (SQLAllocHandle failed)");
		logError (SQL_HANDLE_ENV, sqlEnv, "allocateConnectionHandle", false);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_ENV, sqlEnv, "allocateConnectionHandle (environment)", true);
		logError (SQL_HANDLE_DBC, sqlConnection, "allocateConnectionHandle (connection)", true);

	}

	return sqlConnection;
}


bool setLoginTimeout (SQLHDBC sqlConnection, unsigned long timeout, Key * parentKey)
{
	SQLRETURN ret = SQLSetConnectAttr (sqlConnection, SQL_LOGIN_TIMEOUT, (SQLPOINTER) timeout, 0);

	if (!SQL_SUCCEEDED (ret))
	{
		/* TODO: Extract error and add it to key */
		fprintf (stderr, "Error AllocHDB %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLSetConnectAttr() failed!\nCould not set timeout attribute for ODBC connection!");
		logError (SQL_HANDLE_DBC, sqlConnection, "setLoginTimeout", false);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);

		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_DBC, sqlConnection, "setLoginTimeout", true);
	}


	return true;

}


bool connectToDataSource (SQLHDBC sqlConnection, struct dataSourceConfig dsConfig, Key * parentKey)
{
	SQLRETURN ret = SQLConnect (sqlConnection, (SQLCHAR *) dsConfig.dataSourceName, SQL_NTS, (SQLCHAR *) dsConfig.userName, SQL_NTS, (SQLCHAR *) dsConfig.password, SQL_NTS);

	if (!SQL_SUCCEEDED (ret))
	{
		/* TODO: Extract error and add it to key */
		fprintf (stderr, "Error SqlConnect %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLConnect() failed!\nCould not connect to the ODBC data source %s as user %s! Maybe the data source name, username or password is wrong!", dsConfig.dataSourceName, dsConfig.userName);
		logError (SQL_HANDLE_DBC, sqlConnection, "connectToDataSource", false);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_DBC, sqlConnection, "connectToDataSource", true);
	}


	ELEKTRA_LOG_DEBUG ("Connected to the ODBC data source '%s' as user '%s'.", dsConfig.dataSourceName, dsConfig.userName);
	return true;
}


SQLHSTMT bindSelectParameters (SQLHDBC sqlConnection, struct dataSourceConfig dsConfig, Key * parentKey)
{

	/* Handle for a statement */
	SQLHSTMT sqlStmt = NULL;

	SQLRETURN ret = SQLAllocHandle (SQL_HANDLE_STMT, sqlConnection, &sqlStmt);

	if (!SQL_SUCCEEDED (ret))
	{
		/* TODO: Extract error and add it to key */
		fprintf (stderr, "Error AllocStatement %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate a handle for an SQL statement!");
		logError (SQL_HANDLE_DBC, sqlConnection, "bindSelectParameters",false);


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
		logError (SQL_HANDLE_DBC, sqlConnection, "bindSelectParameters (connection)",true);
		logError (SQL_HANDLE_STMT, sqlStmt, "bindSelectParameters (statement)",true);
	}



	/* Build select statement (+2 for ',' and \0) */
	//unsigned long queryLength = strlen ("SELECT ") + strlen (dsConfig.keyColName) + strlen (dsConfig.valColName) + strlen (" FROM ") + strlen (dsConfig.tableName) + 20;
	/*char * queryString = (char *) malloc (sizeof (char) * queryLength);
	if (!queryString)
	{
		fprintf (stderr, "Could not allocate memory for the query-string for the SQL select-statement!\n");

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return NULL;
	}

	strcpy (queryString, "SELECT ");
	strcat (queryString, dsConfig.keyColName);
	strcat (queryString, ",");
	strcat (queryString, dsConfig.valColName);
	strcat (queryString, " FROM ");
	strcat (queryString, dsConfig.tableName);*/

	/* Prepare the statement that retrieves key-names and string-values */
	ret = SQLPrepare (sqlStmt, (SQLCHAR *) "SELECT keyName, keyValue from elektraKeys", SQL_NTS);



	if (SQL_SUCCEEDED (ret))
	{
		if (ret == SQL_SUCCESS_WITH_INFO)
		{
			logError (SQL_HANDLE_STMT, sqlStmt, "bindSelectParameters", true);
		}

		/* FIXME: Check if free is allowed here! */
		//free (queryString);
		return sqlStmt;

	}
	else
	{
		fprintf (stderr, "Error BindParameter %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate a handle for an SQL statement!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindSelectParameters", false);

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		//free (queryString);
		return NULL;
	}
}


bool bindOutputColumns (SQLHSTMT sqlStmt, SQLCHAR * bufferKeyName, SQLLEN bufferKeyNameSize, SQLCHAR * bufferKeyString, SQLLEN bufferKeyStringSize, SQLLEN * nameLenInd, SQLLEN * stringLenInd, Key * parentKey)
{
	/* TODO: Support for binary keys */
	/* FIXME: Make buffers big enough to store the names and strings (dynamic allocation?) */
	SQLRETURN ret = SQLBindCol (sqlStmt, 1, SQL_C_CHAR, bufferKeyName, bufferKeyNameSize, nameLenInd);


	if (!SQL_SUCCEEDED (ret))
	{
		/* TODO: Extract error and add it to key */
		fprintf (stderr, "Error SqlBindCol1 (char) %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the first column to the keyName!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns", false);

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns", true);
	}



	ret = SQLBindCol (sqlStmt, 2, SQL_C_CHAR, bufferKeyString, bufferKeyStringSize, stringLenInd);


	if (!SQL_SUCCEEDED (ret))
	{
		/* TODO: Extract error and add it to key */
		fprintf (stderr, "Error SqlBindCol2 (int) %d\n", ret);

		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns", false);

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns", true);
	}


	return true;
}


bool executeSelect (SQLHSTMT sqlStmt, Key * parentKey)
{
	SQLRETURN ret = SQLExecute (sqlStmt);

	if (!SQL_SUCCEEDED (ret))
	{
		/* TODO: Extract error and add it to key */
		fprintf (stderr, "Error Select %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLExecDirect() failed!\nCould not execute the query!");
		logError (SQL_HANDLE_STMT, sqlStmt, "executeSelect", false);

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "executeSelect", true);
	}


	return true;
}


KeySet * fetchResults (SQLHSTMT sqlStmt, SQLCHAR * bufferKeyName, SQLCHAR * bufferKeyString, Key * parentKey)
{
	SQLRETURN ret;
	KeySet * ksResult = NULL;

	for (unsigned int i = 1; (ret = SQLFetch (sqlStmt)) != SQL_NO_DATA; i++)
	{
		if (!SQL_SUCCEEDED (ret))
		{
			fprintf (stderr, "Error SQLFetch %d\n", ret);
			logError (SQL_HANDLE_STMT, sqlStmt, "fetchResults", false);

			if (ksResult)
			{
				ksDel (ksResult);
			}


			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			return NULL;
		}
		else
		{
			/* FIXME: remove printf */
			printf ("Processing record number %d\n", i);


			if (ret == SQL_SUCCESS_WITH_INFO)
			{
				logError (SQL_HANDLE_STMT, sqlStmt, "fetchResults", true);
			}

			if (!ksResult)
			{
				ksResult = ksNew (0, KS_END);
			}

			Key * curKey = keyDup (parentKey, KEY_CP_NAME);
			keyAddName (curKey, (char *) bufferKeyName);
			keySetString (curKey, (char *) bufferKeyString);
			ksAppendKey (ksResult, curKey);
		}
	}

	return ksResult;
}


/** Use ODBC to get keys and values from a data source
 * @param dsConfig The configuration of the data source.
 * @param parentkey The keyname must be the mountpoint of the backend, the keynames found in the datasource are appended
 *                  to the keyname of the parent key, the value of the key is not used
 */
KeySet * getKeysFromDataSource (struct dataSourceConfig dsConfig, Key * parentKey)
{
	/* 1. Allocate Environment handle and register version */
	SQLHENV sqlEnv = allocateEnvHandle (parentKey);
	if (!sqlEnv)
	{
		return NULL;
	}

	/* Use ODBC3 */
	if (!setOdbcVersion (sqlEnv, SQL_OV_ODBC3, parentKey))
	{
		return NULL;
	}

	/* 2. Allocate connection handle, set timeout */
	SQLHDBC sqlConnection = allocateConnectionHandle (sqlEnv, parentKey);
	if (!sqlConnection)
	{
		return NULL;
	}

	if (!setLoginTimeout (sqlConnection, 5, parentKey))
	{
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}


	/* 3. Connect to the datasource */
	if (!connectToDataSource (sqlConnection, dsConfig, parentKey))
	{
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	/* 4. Bind input parameters to variables */
	SQLHSTMT sqlStmt = bindSelectParameters (sqlConnection, dsConfig, parentKey);

	if (!sqlStmt)
	{
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	SQLCHAR bufferKeyName[99], bufferKeyString[99];
	SQLLEN nameLenInd, strLenInd;

	/* 5. Bind output columns to variables */
	if (!bindOutputColumns (sqlStmt, bufferKeyName, 99, bufferKeyString, 99, &nameLenInd, &strLenInd, parentKey))
	{
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
	}

	/* 6. Execute the query */
	if (!executeSelect (sqlStmt, parentKey))
	{
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
	}

	/* 7. Fetch results */
	KeySet * ksResult = fetchResults (sqlStmt, bufferKeyName, bufferKeyString, parentKey);

	/* If errors occurred, the statement handle was already freed by the fetchResults()-function. */
	if (ksResult)
	{
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
	}


	SQLDisconnect (sqlConnection);
	ELEKTRA_LOG_DEBUG ("Disconnected from ODBC data source.");

	SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
	SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);

	return ksResult;
}


int ELEKTRA_PLUGIN_FUNCTION (init) (Plugin * plugin ELEKTRA_UNUSED, KeySet * definition ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// init as read-only
	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}


int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * plugin, KeySet * ksReturned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/backendOdbc"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/backendOdbc", KEY_VALUE, "backendOdbc plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/backendOdbc/exports", KEY_END),
			keyNew ("system:/elektra/modules/backendOdbc/exports/init", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (init), KEY_END),
			keyNew ("system:/elektra/modules/backendOdbc/exports/get", KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION (get), KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/backendOdbc/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (ksReturned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	ElektraKdbPhase phase = elektraPluginGetPhase (plugin);
	switch (phase)
	{
	case ELEKTRA_KDB_GET_PHASE_RESOLVER:
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	case ELEKTRA_KDB_GET_PHASE_STORAGE:
	{
		struct dataSourceConfig dsConfig;
		dsConfig.dataSourceName = "Selektra";
		dsConfig.tableName = "elektraKeys";
		dsConfig.keyColName = "keyName";
		dsConfig.valColName = "keyValue";
		dsConfig.userName = "flo";
		dsConfig.password = "elektra";

		KeySet * ksOdbcData = getKeysFromDataSource (dsConfig, parentKey);
		ksAppend (ksReturned, ksOdbcData);


		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	default:
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
}


Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("backend",
		ELEKTRA_PLUGIN_INIT, &ELEKTRA_PLUGIN_FUNCTION (init),
		ELEKTRA_PLUGIN_GET, &ELEKTRA_PLUGIN_FUNCTION (get),
	ELEKTRA_PLUGIN_END);
	// clang-format on
}

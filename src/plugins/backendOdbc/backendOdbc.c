

#include "../backend/backend.h"
#include "backendprivateOdbc.h"

#include <kdberrors.h>
#include <kdblogger.h>
#include <kdbprivate.h>


KeySet * getKeysFromDataSource (SQLCHAR * dsn, SQLCHAR * userName, SQLCHAR * password, Key * parentKey)
{
	/* 1. Allocate Environment handle and register version */
	SQLHENV sqlEnv;
	long ret = SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &sqlEnv);

	if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
	{
		/* TODO: Extract error and add it to key */
		//fprintf (stderr, "Error AllocHandle\n");
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate handle for ODBC environment!");
		return NULL;
	}

	/* Use ODBC3 */
	ret = SQLSetEnvAttr (sqlEnv, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) SQL_OV_ODBC3, 0);

	if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
	{
		/* TODO: Extract error and add it to key */
		//fprintf (stderr, "Error SetEnvAttr\n");
		ELEKTRA_LOG_NOTICE ("SQLSetEnvAttr() failed!\nCould not set ODBC-version attribute for ODBC environment!");
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	/* 2. Allocate connection handle, set timeout */
	SQLHDBC sqlConnection;
	ret = SQLAllocHandle (SQL_HANDLE_DBC, sqlEnv, &sqlConnection);

	if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
	{
		/* TODO: Extract error and add it to key */
		//fprintf (stderr, "Error AllocHDB %ld\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle () failed!\nCould not allocate handle for ODBC connection! (SQLAllocHandle failed)");
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	/* Variables needed for more detailed error information */
	SQLCHAR sqlState[10];
	SQLINTEGER sqlErrCode;
	SQLSMALLINT sqlMsgLen;
	SQLCHAR sqlMsg[200];

	ret = SQLSetConnectAttr (sqlConnection, SQL_LOGIN_TIMEOUT, (SQLPOINTER)5, 0);

	if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
	{
		/* TODO: Extract error and add it to key */
		SQLGetDiagRec (SQL_HANDLE_DBC, sqlConnection, 1, sqlState, &sqlErrCode, sqlMsg, 100, &sqlMsgLen);


		//fprintf (stderr, "Error AllocHDB %ld\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLSetConnectAttr() failed!\nCould not set timeout attribute for ODBC connection!");
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	/* 3. Connect to the datasource */
	ret = SQLConnect (sqlConnection, dsn, SQL_NTS, userName, SQL_NTS, (SQLCHAR *) password, SQL_NTS);

	if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
	{
		/* TODO: Extract error and add it to key */
		//fprintf (stderr, "Error SqlConnect %ld\n", ret);
		SQLGetDiagRec (SQL_HANDLE_DBC, sqlConnection, 1, sqlState, &sqlErrCode, sqlMsg, 100, &sqlMsgLen);

		//fprintf (stderr, "%s (%d)\n", V_OD_msg, V_OD_err);
		ELEKTRA_LOG_NOTICE ("SQLConnect() failed!\nCould not connect to the ODBC data source %s as user %s! Maybe the data source name, username or password is wrong!", dsn, userName);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	ELEKTRA_LOG_DEBUG ("Connected to the ODBC data source '%s' as user '%s'.", dsn, userName);

	/* 4. Bind columns to variables */
	/* Handle for a statement */
	SQLHSTMT sqlStmt;


	/* TODO: Support for binary keys */
	/* FIXME: Make buffers big enough to store the names and strings (dynamic allocation?) */

	/* Key name */
	SQLCHAR bufferKeyName[99];

	/* Key string */
	SQLCHAR bufferKeyString[99];
	SQLLEN retStrLen;


	ret = SQLAllocHandle (SQL_HANDLE_STMT, sqlConnection, &sqlStmt);

	if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
	{
		/* TODO: Extract error and add it to key */
		//fprintf (stderr, "Error AllocStatement %ld\n", ret);

		SQLGetDiagRec (SQL_HANDLE_DBC, sqlConnection, 1, sqlState, &sqlErrCode, sqlMsg, 100, &sqlMsgLen);
		//fprintf (stderr, "%s (%d)\n", V_OD_msg, V_OD_err);

		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate a handle for an SQL statement!");


		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}


	ret = SQLBindCol (sqlStmt, 1, SQL_C_CHAR, &bufferKeyName, 99, &retStrLen);

	if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
	{
		/* TODO: Extract error and add it to key */
		//fprintf (stderr, "Error SqlBindCol1 (char) %ld\n", ret);

		SQLGetDiagRec (SQL_HANDLE_DBC, sqlConnection, 1, sqlState, &sqlErrCode, sqlMsg, 100, &sqlMsgLen);
		//fprintf (stderr, "%s (%d)\n", V_OD_msg, V_OD_err);

		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the first column of data source '%s' to the keyName!", dsn);

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	ret = SQLBindCol (sqlStmt, 2, SQL_C_CHAR, &bufferKeyString, 99, &retStrLen);

	if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
	{
		/* TODO: Extract error and add it to key */
		//fprintf (stderr, "Error SqlBindCol2 (int) %ld\n", ret);

		SQLGetDiagRec (SQL_HANDLE_DBC, sqlConnection, 1, sqlState, &sqlErrCode, sqlMsg, 100, &sqlMsgLen);
		//fprintf (stderr, "%s (%d)\n", V_OD_msg, V_OD_err);

		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column of data source '%s' to the keyString (value of the key)!", dsn);

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	/* 5. Execute the query */
	/* TODO: Allow custom table and column names, customization of query */
	ret = SQLExecDirect (sqlStmt, (SQLCHAR *) "SELECT keyName, keyValue FROM elektraKeys;", SQL_NTS);

	if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
	{
		/* TODO: Extract error and add it to key */
		//fprintf (stderr, "Error Select %ld\n", ret);

		SQLGetDiagRec (SQL_HANDLE_DBC, sqlConnection, 1, sqlState, &sqlErrCode, sqlMsg, 100, &sqlMsgLen);
		//fprintf (stderr, "%s (%d)\n", V_OD_msg, V_OD_err);

		ELEKTRA_LOG_NOTICE ("SQLExecDirect() failed!\nCould not execute the query!");

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	/* 6. Fetch results */
	SQLLEN sqlNumRows;
	ret = SQLRowCount (sqlStmt, &sqlNumRows);

	if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
	{
		/* TODO: Extract error and add it to key */
		//fprintf (stderr, "Error RowCount %ld\n", ret);

		SQLGetDiagRec (SQL_HANDLE_DBC, sqlConnection, 1, sqlState, &sqlErrCode, sqlMsg, 100, &sqlMsgLen);
		//fprintf (stderr, "%s (%d)\n", V_OD_msg, V_OD_err);

		ELEKTRA_LOG_NOTICE ("SQLRowCount() failed!\nCould not count the number of returned rows (=keys)!");

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	ELEKTRA_LOG_DEBUG ("Number of rows: %ld", sqlNumRows);

	KeySet * ksResult = NULL;
	for (int i = 1; (ret = SQLFetch (sqlStmt)) != SQL_NO_DATA; i++)
	{
		if ((ret != SQL_SUCCESS) && (ret != SQL_SUCCESS_WITH_INFO))
		{
			//fprintf (stderr, "Error SQLFetch %ld\n", ret);

			SQLGetDiagRec (SQL_HANDLE_DBC, sqlConnection, 1, sqlState, &sqlErrCode, sqlMsg, 100, &sqlMsgLen);
			//fprintf (stderr, "%s (%d)\n", V_OD_msg, V_OD_err);

			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			SQLDisconnect (sqlConnection);
			SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
			SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
			return NULL;
		}
		else
		{
			if (!ksResult)
			{
				ksResult = ksNew (sqlNumRows, KS_END);
			}

			Key * curKey = keyDup (parentKey, KEY_CP_NAME);

			keyAddName (curKey, (char *) bufferKeyName);
			keySetString (curKey, (char *) bufferKeyString);
			ksAppendKey (ksResult, curKey);
		}
	}

	SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);

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
		/*
		Key * dupParent = keyDup (parentKey, KEY_CP_NAME);
		keySetString (dupParent, "Below is data from an ODBC data source!");
		ksAppendKey (ksReturned, dupParent);

		Key * keyOdbc = keyDup (parentKey, KEY_CP_NAME);
		keyAddBaseName (keyOdbc, "odbcTest");
		keySetString (keyOdbc, "This is a test key for the new ODBC backend plugin!");
		ksAppendKey (ksReturned, keyOdbc);

		Key * keyOdbc2 = keyDup (parentKey, KEY_CP_NAME);
		keyAddBaseName (keyOdbc2, "odbcTest2");
		keySetString (keyOdbc2, "This is a 2nd test key for the new ODBC backend plugin!");
		ksAppendKey (ksReturned, keyOdbc2);*/

		KeySet * ksOdbcData = getKeysFromDataSource ((SQLCHAR *) "Selektra", (SQLCHAR *) "flo", (SQLCHAR *) "elektra", parentKey);
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

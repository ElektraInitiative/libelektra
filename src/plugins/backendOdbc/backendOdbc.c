

#include "../backend/backend.h"
#include "backendprivateOdbc.h"

#include <kdberrors.h>
#include <kdblogger.h>
#include <kdbprivate.h>
#include <kdbassert.h>

/* FIXME: Remove, only for testing during dev */
#include <stdio.h>

#define KEYNAME_BUFFER_SIZE 63
#define METAKEYNAME_BUFFER_SIZE 31
#define KEYSTRING_BUFFER_SIZE 255
#define METASTRING_BUFFER_SIZE KEYSTRING_BUFFER_SIZE


struct dataSourceConfig
{
	char * dataSourceName;
	char * userName;
	char * password;
	char * tableName;
	char * keyColName;
	char * valColName;
	char * metaTableName;
	char * metaTableKeyColName;
	char * metaTableMetaKeyColName;
	char * metaTableMetaValColName;
};


struct columnData
{
	SQLCHAR bufferKeyName[KEYNAME_BUFFER_SIZE];
	SQLLEN nameLenInd;

	SQLCHAR	bufferKeyStr[KEYSTRING_BUFFER_SIZE];
	SQLLEN strLenInd;

	SQLCHAR	bufferMetaKeyName[METAKEYNAME_BUFFER_SIZE];
	SQLLEN metaNameLenInd;

	SQLCHAR bufferMetaKeyStr[METASTRING_BUFFER_SIZE];
	SQLLEN metaStrLenInd;
};


/* make sure to free the returned string */
char * getStringFromBaseName (KeySet * searchKs, Key * lookupKey, const char *baseName, bool addBaseName)
{
	if (addBaseName)
	{
		keyAddBaseName (lookupKey, baseName);
	}
	else
	{
		keySetBaseName (lookupKey, baseName);
	}

	char * resultString = NULL;
	Key * resultKey = ksLookup (searchKs, lookupKey, KDB_O_NONE);

	if (keyGetValueSize (resultKey) > 0)
	{
		resultString = elektraMalloc (keyGetValueSize (resultKey));

		if (resultString)
		{
			if (keyGetString (resultKey, resultString, keyGetValueSize (resultKey)) < 1)
			{
				elektraFree (resultString);
				resultString = NULL;
			}
		}


	}

	return resultString;
}


struct dataSourceConfig * fillDsStructFromKeys (Key * parentKey)
{
	KeySet * ksResults = ksNew (0, KS_END);
	if (!ksResults)
	{
		return NULL;
	}

	KDB * kdb = kdbOpen (NULL, parentKey);
	if (!kdb)
	{
		ksDel (ksResults);
		return NULL;
	}

	if (kdbGet (kdb,ksResults, parentKey) == -1)
	{
		ksDel (ksResults);
		kdbClose (kdb, parentKey);
		return NULL;
	}

	/* Only the call to kdbGet() was needed, we can close the handle now */
	kdbClose (kdb, parentKey);

	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof(struct dataSourceConfig));
	if (!dsConfig)
	{
		ksDel (ksResults);
		return NULL;
	}

	Key * lookupKey = keyDup (parentKey, KEY_CP_NAME);
	if (!lookupKey)
	{
		elektraFree (dsConfig);
		ksDel (ksResults);
		return NULL;
	}



	/* Get configuration data for the ODBC data source */

	bool valueMissing = false;

	dsConfig->dataSourceName = getStringFromBaseName (ksResults, lookupKey, "dataSourceName", true);
	if(!dsConfig->dataSourceName)
	{
		valueMissing = true;
	}

	if (!valueMissing)
	{
		/* Username and password are optional (NULL = no username/password) */
		dsConfig->userName = getStringFromBaseName (ksResults, lookupKey, "userName", false);
		dsConfig->password = getStringFromBaseName (ksResults, lookupKey, "password", false);
		keySetBaseName (lookupKey, "table");

		dsConfig->tableName = getStringFromBaseName (ksResults, lookupKey, "name", true);
		if (!dsConfig->tableName)
		{
			valueMissing = true;
		}
	}

	if (!valueMissing)
	{
		dsConfig->keyColName = getStringFromBaseName (ksResults, lookupKey, "keyColName", false);
		dsConfig->valColName = getStringFromBaseName (ksResults, lookupKey, "valColName", false);

		if (!(dsConfig->keyColName) || !(dsConfig->valColName) ||  (keySetBaseName (lookupKey, NULL) <= 0))
		{
			valueMissing = true;
		}
	}


	if (!valueMissing)
	{
		keySetBaseName (lookupKey, "metaTable");
		dsConfig->metaTableName = getStringFromBaseName (ksResults, lookupKey, "name", true);

		if (dsConfig->metaTableName)
		{
			dsConfig->metaTableKeyColName = getStringFromBaseName (ksResults, lookupKey, "keyColName", false);
			dsConfig->metaTableMetaKeyColName = getStringFromBaseName (ksResults, lookupKey, "metaKeyColName", false);
			dsConfig->metaTableMetaValColName = getStringFromBaseName (ksResults, lookupKey, "metaKeyValColName", false);

			if (!(dsConfig->metaTableKeyColName) || !(dsConfig->metaTableMetaKeyColName) || !(dsConfig->metaTableMetaValColName))
			{
				valueMissing = true;
			}
		}
	}

	keyDel (lookupKey);
	ksDel (ksResults);

	if (valueMissing)
	{
		/* Free als strings in the struct (NULL is ignored by free()) */
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


void logError (SQLSMALLINT handleType, SQLHANDLE handle, char * functionName, bool isInfo, Key * parentKey)
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


		SQLRETURN ret = SQLGetDiagRec (handleType, handle, i, sqlState, &nativeError, errMsg, SQL_MAX_MESSAGE_LENGTH + 1, &errMsgLen);
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

		/* TODO: Extract error and add it to key */
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
		fprintf (stderr, "Error AllocHandle\n");
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
		fprintf (stderr, "Error SetEnvAttr\n");
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
		fprintf (stderr, "Error AllocHDB %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle () failed!\nCould not allocate handle for ODBC connection! (SQLAllocHandle failed)");
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
		fprintf (stderr, "Error AllocHDB %d\n", ret);
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


bool connectToDataSource (SQLHDBC sqlConnection, struct dataSourceConfig dsConfig, Key * parentKey)
{
	SQLRETURN ret = SQLConnect (sqlConnection, (SQLCHAR *) dsConfig.dataSourceName, SQL_NTS, (SQLCHAR *) dsConfig.userName, SQL_NTS, (SQLCHAR *) dsConfig.password, SQL_NTS);

	if (!SQL_SUCCEEDED (ret))
	{
		fprintf (stderr, "Error SqlConnect %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLConnect() failed!\nCould not connect to the ODBC data source %s as user %s! Maybe the data source name, username or password is wrong!", dsConfig.dataSourceName, dsConfig.userName);
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


/** Make sure to free the returned string
 * @param quoteString The characters that should be added before and after identifiers, pass NULL if your identifiers in @dsconfig are already quoted or if you don't want to use quoted identifier
 * TODO: add quotations around identifiers in the query string, check input for valid identifier strings
 * @retval "" (empty string) invalid input detected
 * @retval NULL memory allocation failed
 * @returns the query string for the select query the get keynames, key-values (strings) and metadata from an SQL data source
 */
char * getSelectQueryString (struct dataSourceConfig dsConfig, char * quoteString)
{
		/* A sample query string that shows the structure of the SELECT query that this function generates:
		 * SELECT "elektra"."key", "elektra"."val", "elektrameta"."metakey", "elektrameta"."metaval" FROM {oj "elektra" LEFT OUTER JOIN "elektrameta" ON "elektra"."key"="elektrameta"."key"}
		 */

		/* Verify that all necessary strings are provided */
		if (!dsConfig.tableName || !(*dsConfig.tableName) || !dsConfig.keyColName || !(*dsConfig.keyColName) || !dsConfig.valColName || !(*dsConfig.valColName) || !dsConfig.metaTableName || !(*dsConfig.metaTableName)
			|| !dsConfig.metaTableKeyColName || !(*dsConfig.metaTableKeyColName) || !dsConfig.metaTableMetaKeyColName || !(*dsConfig.metaTableMetaKeyColName) || !dsConfig.metaTableMetaValColName || !(*dsConfig.metaTableMetaValColName))
		{
			return "";
		}


		/* 1. Calculate strlen of all column names */
		size_t sumLen = strlen (dsConfig.keyColName) * 2 + strlen (dsConfig.valColName) + strlen (dsConfig.metaTableMetaKeyColName) + strlen (dsConfig.metaTableMetaValColName) + strlen (dsConfig.metaTableKeyColName);

		/* 2. Add table names (for SELECT and outer join parts of the query string, add 6 bytes for the cases where the column name follows the table name ('.' as separator */
		sumLen += strlen (dsConfig.tableName) * 4 + strlen (dsConfig.metaTableName) * 4 + 6;

		/* 3. Add strlen for static parts of the query + 1 byte for \0 */
		sumLen += strlen ("SELECT , , ,  FROM {oj  LEFT OUTER JOIN  ON =}");

		/* 4. Add bytes for quoting identifiers (table- and column-names) */
		if (quoteString)
		{
			sumLen += (14 * 2 * strlen (quoteString));
		}
		else
		{
			/* Don't use quotes for identifiers */
			quoteString = "";
		}


		char * queryString = elektraMalloc (sizeof (char) * sumLen);

		if (!queryString)
		{
			return NULL;
		}

		/* Build the string */
		char * strEnd = stpcpy (queryString, "SELECT ");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.tableName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, ".");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.keyColName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, ", ");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.tableName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, ".");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.valColName);
		strEnd = stpcpy (strEnd, quoteString);

		strEnd = stpcpy (strEnd, ", ");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.metaTableName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, ".");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.metaTableMetaKeyColName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, ", ");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.metaTableName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, ".");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.metaTableMetaValColName);
		strEnd = stpcpy (strEnd, quoteString);

		strEnd = stpcpy (strEnd, " FROM {oj ");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.tableName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, " LEFT OUTER JOIN ");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.metaTableName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, " ON ");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.tableName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, ".");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.keyColName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, "=");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.metaTableName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, ".");
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, dsConfig.metaTableKeyColName);
		strEnd = stpcpy (strEnd, quoteString);
		strEnd = stpcpy (strEnd, "}");

		return queryString;
}


SQLHSTMT prepareSelectStmt (SQLHDBC sqlConnection, struct dataSourceConfig dsConfig, Key * parentKey)
{

	/* Handle for a statement */
	SQLHSTMT sqlStmt = NULL;

	SQLRETURN ret = SQLAllocHandle (SQL_HANDLE_STMT, sqlConnection, &sqlStmt);

	if (!SQL_SUCCEEDED (ret))
	{
		fprintf (stderr, "Error AllocStatement %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate a handle for an SQL statement!");
		logError (SQL_HANDLE_DBC, sqlConnection, "prepareSelectStmt", false, parentKey);


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
		logError (SQL_HANDLE_DBC, sqlConnection, "prepareSelectStmt (connection)", true, parentKey);
		logError (SQL_HANDLE_STMT, sqlStmt, "prepareSelectStmt (statement)", true, parentKey);
	}


	/* Get driver specific identifier quote character
	 * (see: https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/quoted-identifiers) for more information */

	char identifierQuoteChar[2];
	SQLSMALLINT quoteCharLen = 0;
	ret = SQLGetInfo (sqlConnection, SQL_IDENTIFIER_QUOTE_CHAR, identifierQuoteChar, 2, &quoteCharLen);

	if (!SQL_SUCCEEDED (ret))
	{
		/* treat error as info and try to use the standard value " for quoting identifiers */
		ELEKTRA_LOG_NOTICE ("Could not get an identifier quote char from the driver, using \" as defined by the SQL-92 standard");
		printf ("first getinfo not succeeded");
		identifierQuoteChar[0] = '"';
		identifierQuoteChar[1] = '\0';
	}

	if (!SQL_SUCCEEDED (ret) || ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_DBC, sqlConnection, "prepareSelectStmt (identifierQuoteChar)", true, parentKey);
	}


	char * queryString;
	if (quoteCharLen > 1)
	{
		printf ("quoteCharLen was > 1!");
		ELEKTRA_LOG_WARNING ("Got a string for the info SQL_IDENTIFIER_QUOTE_CHAR with more than one character, this is unusual.");
		char * identifierQuoteStr = elektraMalloc ((quoteCharLen + 1) * sizeof (char));
		ret = SQLGetInfo (sqlConnection, SQL_IDENTIFIER_QUOTE_CHAR, identifierQuoteStr, 1, &quoteCharLen);

		if (!SQL_SUCCEEDED (ret))
		{
			/* treat error as info and try to use the standard value " for quoting identifiers */
			ELEKTRA_LOG_NOTICE ("Could not get an identifier quote string from the driver, despite the driver state that the string for the info SQL_IDENTIFIER_QUOTE_CHAR has more than one character!");
			logError (SQL_HANDLE_DBC, sqlConnection, "prepareSelectStmt (identifierQuoteString)", false, parentKey);

			elektraFree (identifierQuoteStr);
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			SQLDisconnect (sqlConnection);
			SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);

			return NULL;
		}
		else if (ret == SQL_SUCCESS_WITH_INFO)
		{
			logError (SQL_HANDLE_DBC, sqlConnection, "prepareSelectStmt (identifierQuoteString)", true, parentKey);
		}

		queryString = getSelectQueryString (dsConfig, identifierQuoteStr);
		elektraFree (identifierQuoteStr);
	}
	else
	{
		queryString = getSelectQueryString (dsConfig, identifierQuoteChar);
	}

	if (!queryString || !(*queryString))
	{
		if (!queryString)
		{
			fprintf (stderr, "Could not allocate memory for the query-string for the SQL select-statement!\n");
		}
		else
		{
			fprintf (stderr, "The input configuration for the data source contained missing or invalid values.\nPlease check your ODBC and data source configuration!\n");
		}

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		return NULL;
	}

	/* Prepare the statement that retrieves key-names and string-values */
	ret = SQLPrepare (sqlStmt, (SQLCHAR *) queryString, SQL_NTS);

	/* Not a deferred buffer, according to: https://learn.microsoft.com/en-us/sql/odbc/reference/develop-app/deferred-buffers */
	free (queryString);


	if (SQL_SUCCEEDED (ret))
	{
		if (ret == SQL_SUCCESS_WITH_INFO)
		{
			logError (SQL_HANDLE_STMT, sqlStmt, "prepareSelectStmt", true, parentKey);
		}


		return sqlStmt;

	}
	else
	{
		fprintf (stderr, "Error BindParameter %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLAllocHandle() failed!\nCould not allocate a handle for an SQL statement!");
		logError (SQL_HANDLE_STMT, sqlStmt, "prepareSelectStmt", false, parentKey);

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		SQLDisconnect (sqlConnection);
		SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
		//free (queryString);
		return NULL;
	}
}


bool bindOutputColumns (SQLHSTMT sqlStmt, struct columnData * buffers, Key * parentKey)
{
	/* TODO: Support for binary keys */


	/* Bind column 1 (key-name) */
	SQLRETURN ret = SQLBindCol (sqlStmt, 1, SQL_C_CHAR, buffers->bufferKeyName, KEYNAME_BUFFER_SIZE, &(buffers->nameLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		fprintf (stderr, "Error SqlBindCol1 (keyname) %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the first column to the keyName!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns (1)", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns (1)", true, parentKey);
	}


	/* Bind column 2 (key-value) */
	ret = SQLBindCol (sqlStmt, 2, SQL_C_CHAR, buffers->bufferKeyStr, KEYSTRING_BUFFER_SIZE, &(buffers->strLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		fprintf (stderr, "Error SqlBindCol2 (keystring) %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns (2)", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns (2)", true, parentKey);
	}

	/* Bind column 3 (metakey-name) */
	ret = SQLBindCol (sqlStmt, 3, SQL_C_CHAR, buffers->bufferMetaKeyName, METAKEYNAME_BUFFER_SIZE, &(buffers->metaNameLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		fprintf (stderr, "Error SqlBindCol3 (metaname) %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns (3)", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns (3)", true, parentKey);
	}


	/* Bind column 4 (metakey-value) */
	ret = SQLBindCol (sqlStmt, 4, SQL_C_CHAR, buffers->bufferMetaKeyStr, METASTRING_BUFFER_SIZE, &(buffers->metaStrLenInd));
	if (!SQL_SUCCEEDED (ret))
	{
		fprintf (stderr, "Error SqlBindCol4 (metastring) %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLBindCol() failed!\nCould not bind the second column to the keyString (value of the key)!");
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns (4)", false, parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "bindOutputColumns (4)", true, parentKey);
	}

	return true;
}


bool executeSelect (SQLHSTMT sqlStmt, Key * parentKey)
{
	SQLRETURN ret = SQLExecute (sqlStmt);

	if (!SQL_SUCCEEDED (ret))
	{
		fprintf (stderr, "Error Select %d\n", ret);
		ELEKTRA_LOG_NOTICE ("SQLExecDirect() failed!\nCould not execute the query!");
		logError (SQL_HANDLE_STMT, sqlStmt, "executeSelect", false, parentKey);

		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}
	else if (ret == SQL_SUCCESS_WITH_INFO)
	{
		logError (SQL_HANDLE_STMT, sqlStmt, "executeSelect", true, parentKey);
	}


	return true;
}


bool getLongData (SQLHSTMT sqlStmt, SQLUSMALLINT colNumber, SQLSMALLINT targetType, char ** targetValue, SQLLEN bufferSize, Key * parentKey)
{

	SQLLEN getDataLenOrInd;
	SQLRETURN getDataRet;

	unsigned int iteration = 0;

	do
	{
		if (iteration > 0)
		{
			if (elektraRealloc ((void**) targetValue, bufferSize * (iteration + 1)) == -1)
			{
				fprintf (stderr, "Error SQLGetData: Could not reallocate buffer to provide more space (out of memory?)\n");
				SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
				return false;
			}
		}


		getDataRet = SQLGetData (sqlStmt, colNumber, targetType, (*targetValue) + (iteration * (bufferSize-1)), bufferSize, &getDataLenOrInd);

		if (SQL_SUCCEEDED (getDataRet))
		{
			if (getDataRet == SQL_SUCCESS_WITH_INFO)
			{
				logError (SQL_HANDLE_STMT, sqlStmt, "getLongData", true, parentKey);
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
			fprintf (stderr, "Error SQLGetData, getDataRet not succeeded\n");
			logError (SQL_HANDLE_STMT, sqlStmt, "getLongData", false, parentKey);
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			return false;
		}

		iteration++;

	} while (getDataRet != SQL_NO_DATA);


	return true;
}


KeySet * fetchResults (SQLHSTMT sqlStmt, struct columnData * buffers, Key * parentKey)
{
	SQLRETURN ret;
	KeySet * ksResult = NULL;


	/* Needed for detecting if the current row contains a new key or just a further metakey for a previous key (outer join) */
	char * prevKeyName = NULL;
	Key * curKey = NULL;
	Key * prevKey;

	for (unsigned int i = 1; (ret = SQLFetch (sqlStmt)) != SQL_NO_DATA; i++)
	{
		 ELEKTRA_ASSERT (buffers->bufferKeyName && buffers->nameLenInd != SQL_NULL_DATA, "The ODBC-backend retrieved an empty- or null-string for the key-name. This is not allowed! Please check you data source.");


		if (!SQL_SUCCEEDED (ret))
		{
			fprintf (stderr, "Error SQLFetch %d\n", ret);
			logError (SQL_HANDLE_STMT, sqlStmt, "fetchResults", false, parentKey);


			elektraFree (prevKeyName);

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
			//printf ("NameLenInd: %ld, StrLenInd: %ld\n", *nameLenInd, *stringLenInd);

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
						/* No information about the total length is available --> The new buffer maybe must be resized multiple times */
						longKeyName = (char *) elektraMalloc (sizeof (char) * (KEYNAME_BUFFER_SIZE * 2));
						retGetLongData = getLongData (sqlStmt, 1, SQL_C_CHAR, &longKeyName, KEYNAME_BUFFER_SIZE * 2, parentKey);
					}
					else if (KEYNAME_BUFFER_SIZE <= buffers->nameLenInd)
					{
						longKeyName = (char *) elektraMalloc (sizeof (char) * (buffers->nameLenInd + 1));
						retGetLongData = getLongData (sqlStmt, 1, SQL_C_CHAR, &longKeyName, (buffers->nameLenInd + 1), parentKey);
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
						retGetLongData = getLongData (sqlStmt, 2, SQL_C_CHAR, &longKeyString, KEYSTRING_BUFFER_SIZE * 2, parentKey);
					}
					else if (!isFurtherMetaKey && KEYSTRING_BUFFER_SIZE <= buffers->strLenInd)
					{
						longKeyString = (char *) elektraMalloc (sizeof (char) * (buffers->strLenInd + 1));
						retGetLongData = getLongData (sqlStmt, 2, SQL_C_CHAR, &longKeyString, (buffers->strLenInd + 1), parentKey);
					}

					/* Check metakey-name column */
					if (retGetLongData && buffers->metaNameLenInd == SQL_NO_TOTAL)
					{
						longMetaKeyName = (char *) elektraMalloc (sizeof (char) * (METAKEYNAME_BUFFER_SIZE * 2));
						retGetLongData = getLongData (sqlStmt, 3, SQL_C_CHAR, &longMetaKeyName, METAKEYNAME_BUFFER_SIZE * 2, parentKey);
					}
					else if (METAKEYNAME_BUFFER_SIZE <= buffers->metaNameLenInd)
					{
						longMetaKeyName = (char *) elektraMalloc (sizeof (char) * (buffers->metaNameLenInd + 1));
						retGetLongData = getLongData (sqlStmt, 3, SQL_C_CHAR, &longMetaKeyName, (buffers->metaNameLenInd + 1), parentKey);
					}


					/* Check metakey-string column */
					if (retGetLongData && buffers->metaStrLenInd == SQL_NO_TOTAL)
					{
						longMetaKeyString = (char *) elektraMalloc (sizeof (char) * (METASTRING_BUFFER_SIZE * 2));
						retGetLongData = getLongData (sqlStmt, 4, SQL_C_CHAR, &longMetaKeyString, METASTRING_BUFFER_SIZE * 2, parentKey);
					}
					else if (METASTRING_BUFFER_SIZE <= buffers->metaStrLenInd)
					{
						longMetaKeyString = (char *) elektraMalloc (sizeof (char) * (buffers->metaStrLenInd + 1));
						retGetLongData = getLongData (sqlStmt, 4, SQL_C_CHAR, &longMetaKeyString, (buffers->metaStrLenInd + 1), parentKey);
					}


					if (!retGetLongData)
					{
						//if (longKeyName)
						//{
						//	ELEKTRA_ASSERT (prevKeyName == longKeyName, "The prevKeyName doesn't point to the current longKeyName, this looks like a programming errors!\nPlease report this issue at https://issues.libelektra.org");
						//	elektraFree (longKeyName);
						//}
						//else
						//{

						/* The variable longKeyName should not be freed here, it either points to the same memory as prevKeyName or is NULL */
						elektraFree (prevKeyName);
						//}
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
					logError (SQL_HANDLE_STMT, sqlStmt, "fetchResults", true, parentKey);
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
				ELEKTRA_ASSERT (curKey, "The flag the indicates that a further metakey is present was set, but the current key was NULL. This is likely a programming error.\nPlease report this issues at https://issues.libelektra.org");
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
				//printf ("longMetaKeyString: %s, buffer: %s", longMetaKeyString ? longMetaKeyString : "", buffers->bufferMetaKeyStr);
				ELEKTRA_ASSERT (!longMetaKeyString && !(buffers->bufferMetaKeyStr[0]), "No metakey-name was found, but a metakey-string, maybe the datasource contains invalid data, please check your datasource!");
			}


			if (metaKeyName)
			{
				if (longMetaKeyString || buffers->bufferMetaKeyStr[0])
				{
					if (longMetaKeyString)
					{
						keySetMeta(curKey, metaKeyName, longMetaKeyString);
						elektraFree (longMetaKeyString);
					}
					else
					{
						keySetMeta(curKey, metaKeyName, (const char *) buffers->bufferMetaKeyStr);
					}
				}
				else
				{
					keySetMeta(curKey, metaKeyName, "");
				}
			}

			elektraFree (longMetaKeyName);



			if (prevKey && curKey != prevKey)
			{
				/* Previous key (incl. all metakeys) is finished --> add it to keyset */
				ksAppendKey (ksResult, prevKey);
			}


			buffers->bufferKeyName[0] = 0;
			buffers->bufferKeyStr[0] = 0;
			buffers->bufferMetaKeyName[0] = 0;
			buffers->bufferMetaKeyStr[0] = 0;
		}
	}

	elektraFree (prevKeyName);

	if (curKey && curKey != prevKey)
	{
		/* Add last key */
		ksAppendKey (ksResult, curKey);
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
	SQLHSTMT sqlStmt = prepareSelectStmt (sqlConnection, dsConfig, parentKey);

	if (!sqlStmt)
	{
		SQLFreeHandle (SQL_HANDLE_ENV, sqlEnv);
		return NULL;
	}

	struct columnData outputBuffers;
	outputBuffers.bufferKeyName[0] = 0;
	outputBuffers.bufferKeyStr[0] = 0;
	outputBuffers.bufferMetaKeyName[0] = 0;
	outputBuffers.bufferMetaKeyStr[0] = 0;
	outputBuffers.nameLenInd = 0;
	outputBuffers.strLenInd = 0;
	outputBuffers.metaNameLenInd = 0;
	outputBuffers.metaStrLenInd = 0;

	/* 5. Bind output columns to variables */
	if (!bindOutputColumns (sqlStmt, &outputBuffers, parentKey))
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
	KeySet * ksResult = fetchResults (sqlStmt, &outputBuffers, parentKey);

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
		dsConfig.dataSourceName = "Pelektra";
		dsConfig.tableName = "elektra";
		dsConfig.keyColName = "key";
		dsConfig.valColName = "val";
		dsConfig.userName = "flo";
		dsConfig.password = "elektra";
		dsConfig.metaTableName = "elektrameta";
		dsConfig.metaTableKeyColName = "key";
		dsConfig.metaTableMetaKeyColName = "metakey";
		dsConfig.metaTableMetaValColName = "metaval";

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

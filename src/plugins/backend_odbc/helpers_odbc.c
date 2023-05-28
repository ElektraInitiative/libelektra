#include "backendprivate_odbc.h"
#include <stdio.h>
#include <kdbassert.h>

unsigned char getNumDigits (int i)
{
	unsigned char digits = 1;

	while ((i /= 10) != 0)
	{
		digits++;
	}

	return digits;
}


char ** extractOdbcErrors (char * fn, SQLHANDLE odbcHandle, SQLSMALLINT type)
{
	SQLINTEGER nativeErrCode;
	SQLCHAR state[SQL_SQLSTATE_SIZE + 1];
	SQLCHAR * text = NULL;
	SQLSMALLINT lenText;
	SQLSMALLINT maxLenText = 0;
	SQLRETURN ret;
	char ** resultArray;
	unsigned short msgCount;

	/* two iterations, 1st to determine length of string, 2nd for actually creating the string */
	for (int i = 0; i < 2; i++)
	{
		if (i)
		{
			text = elektraMalloc (maxLenText + 1);
			resultArray = elektraMalloc (msgCount * sizeof (char *));
		}

		SQLINTEGER recNum = 1;
		for (msgCount = 0; SQL_SUCCEEDED (ret = SQLGetDiagRec (type, odbcHandle, recNum++, state,&nativeErrCode,
								       text, i ? maxLenText + 1 : 0, &lenText)); msgCount++)
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
			elektraFree(text);
			return resultArray;
		}
	}

	return NULL;
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


char ** getAvailableDataSources (void)
{
	SQLHENV env;

	SQLCHAR * dsn = NULL;
	SQLSMALLINT lenDsn;
	SQLSMALLINT maxLenDsn = 0;
	unsigned short dsnCount;
	SQLRETURN ret;
	char ** result;

	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &env);
	SQLSetEnvAttr (env, SQL_ATTR_ODBC_VERSION, (SQLPOINTER) SQL_OV_ODBC3, 0);

	/* 2 runs, 1st for determining needed string buffer sizes, 2nd for actually retrieving the strings */
	for (int i = 0; i < 2; i++)
	{
		if (i)
		{
			result = elektraMalloc (dsnCount * sizeof (char *));
			/* add one byte for \0 */
			dsn = elektraMalloc((maxLenDsn + 1) * sizeof (SQLCHAR));
		}

		dsnCount = 0;
		for (SQLUSMALLINT direction = SQL_FETCH_FIRST;
		     SQL_SUCCEEDED (ret = SQLDataSources (env, direction, dsn, i ? maxLenDsn + 1 : 0, &lenDsn, NULL, 0, NULL));
		     direction = SQL_FETCH_NEXT)
		{
			if (i)
			{
				/* FIXME: Check if cast from 'unsigned char' to 'char' is safe here */
				result[dsnCount] =  (char *) dsn;
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


/* make sure to free the returned string */
static char * lookupStringFromKs (KeySet * ks, const char * keyName)
{
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
	ELEKTRA_ASSERT (ret != 0, "keyGetString() returned 0! This should not have happened! Please report the bug at https://issues.libelektra.org");

	if (ret == -1)
	{
		elektraFree (resultString);
		return NULL;
	}

	return resultString;
}


struct dataSourceConfig * fillDsStructFromDefintionKs (KeySet * ksDefinition)
{

	if (!ksDefinition)
	{
		return NULL;
	}

	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof(struct dataSourceConfig));
	if (!dsConfig)
	{
		return NULL;
	}

	/* Get configuration data for the ODBC data source */
	bool valueMissing = false;


	dsConfig->dataSourceName = lookupStringFromKs (ksDefinition, "system:/dataSourceName");

	if(!(dsConfig->dataSourceName))
	{
		valueMissing = true;
	}

	if (!valueMissing)
	{
		/* Username and password are optional (NULL = no username/password) */
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

			if (!(dsConfig->metaTableKeyColName) || !(dsConfig->metaTableMetaKeyColName) || !(dsConfig->metaTableMetaValColName))
			{
				valueMissing = true;
			}
		}
	}


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


char * dsConfigToString (struct dataSourceConfig * dsConfig)
{
	if (!dsConfig)
	{
		return NULL;
	}

	size_t dsConfigStrLen = (dsConfig->dataSourceName ? strlen (dsConfig->dataSourceName) : 0)
	       	//+ (dsConfig.userName ? strlen (dsConfig.userName) : 0)
	       	//+ (dsConfig.password ? strlen (dsConfig.password) : 0)
	       	+ (dsConfig->tableName ? strlen (dsConfig->tableName) : 0)
	       	+ (dsConfig->keyColName ? strlen (dsConfig->keyColName) : 0)
	       	+ (dsConfig->valColName ? strlen (dsConfig->valColName) : 0)
	       	+ (dsConfig->metaTableName ? strlen (dsConfig->metaTableName) : 0)
	       	+ (dsConfig->metaTableKeyColName ? strlen (dsConfig->metaTableKeyColName) : 0)
	       	+ (dsConfig->metaTableMetaKeyColName ? strlen (dsConfig->metaTableMetaKeyColName) : 0)
	       	+ (dsConfig->metaTableMetaValColName ? strlen (dsConfig->metaTableMetaValColName) : 0);

	if (dsConfigStrLen == 0)
	{
		return NULL;
	}

	char * retStr = elektraMalloc (dsConfigStrLen * sizeof (char));

	if (!retStr)
	{
		return NULL;
	}

	char * curPart = retStr;

	curPart = (dsConfig->dataSourceName ? stpcpy (curPart, dsConfig->dataSourceName) : curPart);
	//curPart = (dsConfig.userName ? stpcpy (curPart, dsConfig.userName) : curPart);
	//curPart = (dsConfig.password ? stpcpy (curPart, dsConfig.password) : curPart);
	curPart = (dsConfig->tableName ? stpcpy (curPart, dsConfig->tableName) : curPart);
	curPart = (dsConfig->keyColName ? stpcpy (curPart, dsConfig->keyColName) : curPart);
	curPart = (dsConfig->valColName ? stpcpy (curPart, dsConfig->valColName) : curPart);
	curPart = (dsConfig->metaTableName ? stpcpy (curPart, dsConfig->metaTableName) : curPart);
	curPart = (dsConfig->metaTableKeyColName ? stpcpy (curPart, dsConfig->metaTableKeyColName) : curPart);
	curPart = (dsConfig->metaTableMetaKeyColName ? stpcpy (curPart, dsConfig->metaTableMetaKeyColName) : curPart);
	curPart = (dsConfig->metaTableMetaValColName ? stpcpy (curPart, dsConfig->metaTableMetaValColName) : curPart);

	return retStr;
}

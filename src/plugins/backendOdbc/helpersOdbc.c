#include "backendprivateOdbc.h"
#include <stdio.h>

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



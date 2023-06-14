/**
 * @file
 *
 * @brief Functions for writing data to an ODBC data source.
 *	Insert- and update update operations are supported.
 *
 * This file contains all functions that are especially needed for setting data on a data source.
 * This includes building strings for INSERT INTO and UPDATE queries, preparing and executing queries and sending data.
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

/* FIXME: remove include, only for output during dev */
#include <stdio.h>

/* ODBC related includes */
// #include <sql.h>
#include <sqlext.h>


/**
 *
 *
 * @param deletedKeys
 * @param tableName
 * @param keyColName
 * @param errorKey
 * @return The query string with the '?' parameters for binding the parameters to the query
 * 	Make sure to free the returned string.
 */
/* TODO: add quoted identifiers */
static char * getDeleteQuery (unsigned int numKeys, const char * tableName, const char * keyColName)
{
	if (numKeys == 0)
	{
		return NULL;
	}

	size_t queryStrLen = strlen ("DELETE FROM ") + strlen (tableName) + strlen (" WHERE ") + strlen (keyColName) + strlen (" IN ()");

	/* One comma less than number of keys in the keyset is required, so we've already counted the char for \0 */
	queryStrLen += strlen ("?,") * numKeys;

	char * resultStr = elektraMalloc (queryStrLen * sizeof (char));

	if (!resultStr)
	{
		return NULL;
	}

	char * strEnd = stpcpy (resultStr, "DELETE FROM ");

	strEnd = stpcpy (strEnd, tableName);
	strEnd = stpcpy (strEnd, " WHERE ");
	strEnd = stpcpy (strEnd, keyColName);
	strEnd = stpcpy (strEnd, " IN (");

	for (unsigned long it = 0; it < numKeys; it++)
	{
		if (it < (numKeys - 1))
		{
			strEnd = stpcpy (strEnd, "?,");
		}
		else
		{
			/* last iteration */
			stpcpy (strEnd, "?)");
		}
	}
	return resultStr;
}


/* TODO: add quoted identifiers */
/* make sure to free the returned string */
static char * getUpdateQuery (const struct dataSourceConfig * dsConfig, bool forMetaTable)
{
	if (forMetaTable)
	{
		return elektraFormat ("UPDATE %s SET %s=? WHERE %s=? AND %s=?", dsConfig->metaTableName, dsConfig->metaTableMetaValColName,
				      dsConfig->metaTableKeyColName, dsConfig->metaTableMetaKeyColName);
	}
	else
	{
		return elektraFormat ("UPDATE %s SET %s=? WHERE %s=?", dsConfig->tableName, dsConfig->valColName, dsConfig->keyColName);
	}
}

/* TODO: add quoted identifiers */
/* make sure to free the returned string */
static char * getInsertQuery (const struct dataSourceConfig * dsConfig, bool forMetaTable)
{
	if (forMetaTable)
	{
		return elektraFormat ("INSERT INTO %s (%s, %s, %s) VALUES (?,?,?)", dsConfig->metaTableName, dsConfig->metaTableKeyColName,
				      dsConfig->metaTableMetaKeyColName, dsConfig->metaTableMetaValColName);
	}
	else
	{
		/* Set value first to have to same parameter order as for the UPDATE query */
		return elektraFormat ("INSERT INTO %s (%s, %s) VALUES (?,?)", dsConfig->tableName, dsConfig->valColName,
				      dsConfig->keyColName);
	}
}

/* make sure that you only pass allocated char buffers that are not freed until the statement for which the values are bound is not executed
 * any more or other values are bound to the statement */
static bool bindStringsToStatementV (SQLHSTMT sqlStmt, Key * errorKey, unsigned int numParams, va_list argList)
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
			ret = SQLBindParameter (sqlStmt, u + 1, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR, 0, 0, NULL, 0,
						(SQLLEN *) SQL_NULL_DATA);
		}
		else
		{
			ret = SQLBindParameter (sqlStmt, u + 1, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR, strlen (curStr), 0,
						(SQLCHAR *) curStr, (SQLLEN) strlen (curStr) + 1, NULL);
		}

		printf ("Bound value '%s' to statement\n", curStr);

		if (!SQL_SUCCEEDED (ret))
		{
			printf ("SQLBindParameter failed!\n");
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

static bool bindStringsToStatement (SQLHSTMT sqlStmt, Key * errorKey, unsigned int numParams, ...)
{
	va_list argList;
	va_start (argList, numParams);
	bool ret = bindStringsToStatementV (sqlStmt, errorKey, numParams, argList);
	va_end (argList);
	return ret;
}

/* Make sure to bind exactly the number of parameters that match the number of the placeholder which the query-string you want to execute
 * what the given statement handle has */
static bool bindRelativeKeyNamesToStatement (SQLHSTMT sqlStmt, const KeySet * keysToBind, Key * parentKey)
{
	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided statement handle must not be NULL.");
		return false;
	}

	if (ksGetSize (keysToBind) < 1)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The KeySet with the keys to bind must not be null or empty.");
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}

	if (ksGetSize (keysToBind) > USHRT_MAX)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey,
					      "The maximum number of parameter values to bind is %hu, but you specified a number of %zd",
					      USHRT_MAX, ksGetSize (keysToBind));
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}

	for (elektraCursor it = 0; it < ksGetSize (keysToBind); it++)
	{
		const char * curRelKeyName = elektraKeyGetRelativeName (ksAtCursor (keysToBind, it), parentKey);
		SQLRETURN ret = SQLBindParameter (sqlStmt, it + 1, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR, strlen (curRelKeyName), 0,
						  (SQLCHAR *) curRelKeyName, (SQLLEN) strlen (curRelKeyName) + 1, NULL);

		printf ("Bound value '%s' to statement\n", curRelKeyName);

		if (!SQL_SUCCEEDED (ret))
		{
			printf ("SQLBindParameter failed!\n");
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


SQLLEN removeRows (SQLHSTMT sqlStmt, const KeySet * ksRemovedKeys, const char * tableName, const char * keyColName, bool bindParams,
		   Key * parentKey)
{
	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided statement handle must not be NULL.");
		return -1;
	}

	if (bindParams && (!bindRelativeKeyNamesToStatement (sqlStmt, ksRemovedKeys, parentKey)))
	{
		/* sqlStmt was already freed by bindRelativeKeyNamesToStatement() */
		/* bindRelativeKeyNamesToStatement() should've set an error in the parentKey */
		return -1;
	}

	/* Now our statement handle is ready to use */

	/* TODO: add configuration option for data sources that have cascading delete enabled
	 * --> no extra deletion of tuples in the metatable required */

	char * queryDelete = getDeleteQuery (ksGetSize (ksRemovedKeys), tableName, keyColName);
	if (!queryDelete)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return -1;
	}

	/* Execute the delete query (not persisted until transaction is committed!) */
	SQLLEN affectedRows = executeQuery (sqlStmt, queryDelete, parentKey);
	elektraFree (queryDelete);

	if (affectedRows == -1)
	{
		/* error, statement handle got freed by executeQuery() */
		return -1;
	}

	return affectedRows;
}


static SQLLEN insertOrUpdateRows (SQLHSTMT sqlStmt, const KeySet * ks, const struct dataSourceConfig * dsConfig, bool update,
				  Key * parentKey)
{
	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided statement handle must not be NULL.");
		return -1;
	}

	char * query;

	if (update)
	{
		query = getUpdateQuery (dsConfig, false);
	}
	else
	{
		query = getInsertQuery (dsConfig, false);
	}

	if (!query)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return -1;
	}

	SQLLEN curAffectedRows;

	/* The value -2 indicates that the number of affected rows could not be retrieved from the data source */
	SQLLEN sumAffectedRows = -2;
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * curKey = ksAtCursor (ks, it);

		/* UPDATE table set val=? where key=?
		 * --> we have to bind the value first and then the key-name for which the value should be set */
		if (!bindStringsToStatement (sqlStmt, parentKey, 2, keyString (curKey), elektraKeyGetRelativeName (curKey, parentKey)))
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

	elektraFree (query);
	return sumAffectedRows;
}


static void printChangedKeys (ElektraDiff * diffSet, Key * parentKey)
{
	printf ("Added keys:\n");
	KeySet * ksDiff = elektraDiffGetAddedKeys (diffSet);
	for (elektraCursor it = 0; it < ksGetSize (ksDiff); it++)
	{
		Key * curKey = ksAtCursor (ksDiff, it);
		printf ("keyname: %s, string: %s, relative: %s\n", keyName (curKey), keyString (curKey),
			elektraKeyGetRelativeName (curKey, parentKey));
	}
	ksDel (ksDiff);

	printf ("Removed keys:\n");
	ksDiff = elektraDiffGetRemovedKeys (diffSet);
	for (elektraCursor it = 0; it < ksGetSize (ksDiff); it++)
	{
		Key * curKey = ksAtCursor (ksDiff, it);
		printf ("keyname: %s, string: %s, relative: %s\n", keyName (curKey), keyString (curKey),
			elektraKeyGetRelativeName (curKey, parentKey));
	}
	ksDel (ksDiff);

	printf ("Modified keys:\n");
	ksDiff = elektraDiffGetModifiedKeys (diffSet);
	for (elektraCursor it = 0; it < ksGetSize (ksDiff); it++)
	{
		Key * curKey = ksAtCursor (ksDiff, it);
		printf ("keyname: %s, string: %s, relative: %s\n", keyName (curKey), keyString (curKey),
			elektraKeyGetRelativeName (curKey, parentKey));
	}
	ksDel (ksDiff);
}

/**
 *
 * Please note that this functions starts a new transaction, but does NOT commit it.
 * If you want to finish the transaction (commit or rollback), you have to either call endTransaction() or the related functions
 * of the ODBC API directly.
 *
 * @param dsConfig
 * @param ksData
 * @param errorKey
 * @return
 */
SQLLEN storeKeysInDataSource (struct odbcSharedData * sharedData, KeySet * ks, Key * parentKey)
{

	/* Arguments are checked inside getKeysFromDataSource() */

	/* 1. Get the current data from the data source */
	/* This starts a new transaction that is kept open. The handles for the ODBC environment and connection are stored in sharedData */
	KeySet * ksDs = getKeysFromDataSource (sharedData, true, parentKey);

	/* 2. Compare the data from the data source with the KeySet which should be persisted, create an ElektraDiff */
	ElektraDiff * diffSet = elektraDiffCalculate (ks, ksDs, parentKey);

	/* TODO: Check if delete is safe here */
	ksDel (ksDs);

	if (!diffSet || elektraDiffIsEmpty (diffSet))
	{
		/* no update */
		elektraDiffDel (diffSet);
		return 0;
	}

#if DEBUG
	printChangedKeys (diffSet, parentKey);
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
			ksDel (ksRemovedKeys);
			elektraDiffDel (diffSet);
			return -1;
		}

		/* At first delete the rows from the meta table */
		SQLLEN curAffectedRows = removeRows (sqlStmt, ksRemovedKeys, sharedData->dsConfig->metaTableName,
						     sharedData->dsConfig->metaTableKeyColName, true, parentKey);
		if (curAffectedRows == -1)
		{
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
					      false, parentKey);
		ksDel (ksRemovedKeys);

		if (curAffectedRows == -1)
		{
			elektraDiffDel (diffSet);
			return -1;
		}
		else if (curAffectedRows > 0)
		{
			sumAffectedRows += curAffectedRows;
		}
	}

	KeySet * ksModifiedKeys = elektraDiffGetModifiedKeys (diffSet);

	if (ksGetSize (ksModifiedKeys) > 0)
	{
		if (!sqlStmt)
		{
			sqlStmt = allocateStatementHandle (sharedData->connection, parentKey);
			if (!sqlStmt)
			{
				ksDel (ksModifiedKeys);
				elektraDiffDel (diffSet);
				return -1;
			}
		}

		SQLLEN curAffectedRows = insertOrUpdateRows (sqlStmt, ksModifiedKeys, sharedData->dsConfig, true, parentKey);
		ksDel (ksModifiedKeys);

		if (curAffectedRows == -1)
		{
			elektraDiffDel (diffSet);
			return -1;
		}
		else if (curAffectedRows > 0)
		{
			sumAffectedRows += curAffectedRows;
		}

		ksDel (ksModifiedKeys);
	}

	KeySet * ksAddedKeys = elektraDiffGetAddedKeys (diffSet);

	if (ksGetSize (ksAddedKeys) > 0)
	{
		if (!sqlStmt)
		{
			sqlStmt = allocateStatementHandle (sharedData->connection, parentKey);
			if (!sqlStmt)
			{
				ksDel (ksAddedKeys);
				elektraDiffDel (diffSet);
				return -1;
			}
		}

		SQLLEN curAffectedRows = insertOrUpdateRows (sqlStmt, ksAddedKeys, sharedData->dsConfig, false, parentKey);
		ksDel (ksAddedKeys);

		if (curAffectedRows == -1)
		{
			elektraDiffDel (diffSet);
			return -1;
		}
		else if (curAffectedRows > 0)
		{
			sumAffectedRows += curAffectedRows;
		}

		ksDel (ksAddedKeys);
	}

	/* No disconnecting or freeing here, the handle to the connection and the SQL environment are needed until
	 * the transaction is committed or rolled back. */
	ELEKTRA_LOG_DEBUG ("Finished executing queries in open transaction, make sure to end the transaction.");
	return sumAffectedRows;
}

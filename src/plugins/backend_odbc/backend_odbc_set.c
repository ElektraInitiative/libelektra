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

/* FIXME: remove include, only for output during dev */
#include <kdbassert.h>
#include <kdbdiff.h>
#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <stdio.h>
#include <string.h>

/* ODBC related includes */
// #include <sql.h>
#include <sqlext.h>


/*
static char * getInsertQuery (const KeySet * insertedKeys)
{
	if (!insertedKeys)
	{
		return NULL;
	}
}

static char * getUpdateQuery (const KeySet * modifiedKeys)
{
	if (!modifiedKeys)
	{
		return NULL;
	}
}*/

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
static char * getDeleteQuery (const KeySet * deletedKeys, const char * tableName, const char * keyColName, Key * errorKey)
{
	if (!deletedKeys)
	{
		return NULL;
	}

	size_t queryStrLen = strlen ("DELETE FROM ") + strlen (tableName) + strlen (" WHERE ") + strlen (keyColName) + strlen (" IN ()");

	/* One comma less than number of keys in the keyset is required, so we've already counted the byte for \0 */
	queryStrLen += strlen ("?,") * ksGetSize (deletedKeys);

	char * resultStr = elektraMalloc (queryStrLen * sizeof (char));

	if (!resultStr)
	{
		ELEKTRA_SET_OUT_OF_MEMORY_ERROR (errorKey);
		return NULL;
	}

	char * strEnd = stpcpy (resultStr, "DELETE FROM ");

	strEnd = stpcpy (strEnd, tableName);
	strEnd = stpcpy (strEnd, " WHERE ");
	strEnd = stpcpy (strEnd, keyColName);


	strEnd = stpcpy (strEnd, " IN (");

	for (elektraCursor it = 0; it < ksGetSize (deletedKeys); it++)
	{
		if (it < (ksGetSize (deletedKeys) - 1))
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


/**
 *
 * @param sqlConnection
 * @param deletedKeys
 * @param errorKey
 * @return
 */
static SQLHSTMT getBoundDeleteStmt (SQLHDBC sqlConnection, const KeySet * deletedKeys, Key * errorKey)
{
	/* Handle for a statement */
	SQLHSTMT sqlStmt = allocateStatementHandle (sqlConnection, errorKey);

	if (!sqlStmt)
	{
		return NULL;
	}

	for (elektraCursor it = 0; it < ksGetSize (deletedKeys); it++)
	{
		const char * curRelKeyName = elektraKeyGetRelativeName (ksAtCursor (deletedKeys, it), errorKey);
		SQLRETURN ret = SQLBindParameter (sqlStmt, it + 1, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR, strlen (curRelKeyName), 0,
						  (SQLCHAR *) curRelKeyName, (SQLLEN) strlen (curRelKeyName) + 1, NULL);

		printf ("Bound delete value: %s\n", curRelKeyName);

		if (ret != SQL_SUCCESS)
		{
			printf ("SQLBindParameter failed!\n");
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			SQLDisconnect (sqlConnection);
			SQLFreeHandle (SQL_HANDLE_DBC, sqlConnection);
			return NULL;
		}
	}

	return sqlStmt;
}

static SQLHSTMT getBoundUpdateStmt (SQLHDBC sqlConnection ELEKTRA_UNUSED, const struct dataSourceConfig * dsConfig ELEKTRA_UNUSED,
				    const KeySet * modifiedKeys ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	/* TODO: implement function */
	/* Handle for a statement */
	SQLHSTMT sqlStmt = allocateStatementHandle (sqlConnection, errorKey);

	/*
	if (!sqlStmt)
	{
		return NULL;
	}

	for (elektraCursor it = 0; it < ksGetSize (modifiedKeys))
	{

	}*/


	return sqlStmt;
}


static SQLHSTMT getBoundInsertStmt (SQLHDBC sqlConnection ELEKTRA_UNUSED, const struct dataSourceConfig * dsConfig ELEKTRA_UNUSED,
				    const KeySet * insertedKeys ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	/* TODO: Implement function */
	/* Handle for a statement */
	SQLHSTMT sqlStmt = NULL; // allocateStatementHandle (sqlConnection, errorKey);

	/*
	if (!sqlStmt)
	{
		return NULL;
	}*/

	return sqlStmt;
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
long storeKeysInDataSource (struct odbcSharedData * sharedData, KeySet * ks, Key * parentKey)
{

	/* TODO: Remove checks, already gets checked inside getKeysFromDataSource() */
	/*
	if (!sharedData || !(sharedData->dsConfig))
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided shared data or data source configuration was NULL!");
		return -1;
	}


	if (sharedData->environment)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The environment hande in the provided shared data was NOT NULL! "
					     "This value should not be set before this function is called!");
		return -1;
	}

	if (sharedData->connection)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The connection hande in the provided shared data was NOT NULL! "
					     "This value should not be set before this function is called!");
		return -1;
	}*/

	/* 1. Get the current data from the data source */
	/* This starts a new transaction that is kept open. The handles for the ODBC environment and connection are stored in sharedData */
	KeySet * ksDs = getKeysFromDataSource (sharedData, true, parentKey);

	/* 2. Compare the data from the data source with the KeySet which should be persisted, create an ElektraDiff */
	ElektraDiff * diffSet = elektraDiffCalculate (ks, ksDs, parentKey);

	if (!diffSet || elektraDiffIsEmpty (diffSet))
	{
		/* no update */
		elektraDiffDel (diffSet);
		return 0;
	}

	printf ("Added keys:\n");
	KeySet * ksDbgDiff = elektraDiffGetAddedKeys (diffSet);
	for (elektraCursor it = 0; it < ksGetSize (ksDbgDiff); it++)
	{
		Key * curKey = ksAtCursor (ksDbgDiff, it);
		printf ("keyname: %s, string: %s, relative: %s\n", keyName (curKey), keyString (curKey),
			elektraKeyGetRelativeName (curKey, parentKey));
	}


	ksDel (ksDbgDiff);

	printf ("Removed keys:\n");
	ksDbgDiff = elektraDiffGetRemovedKeys (diffSet);
	for (elektraCursor it = 0; it < ksGetSize (ksDbgDiff); it++)
	{
		Key * curKey = ksAtCursor (ksDbgDiff, it);
		printf ("keyname: %s, string: %s, relative: %s\n", keyName (curKey), keyString (curKey),
			elektraKeyGetRelativeName (curKey, parentKey));
	}
	ksDel (ksDbgDiff);


	printf ("Modified keys:\n");
	ksDbgDiff = elektraDiffGetModifiedKeys (diffSet);
	for (elektraCursor it = 0; it < ksGetSize (ksDbgDiff); it++)
	{
		Key * curKey = ksAtCursor (ksDbgDiff, it);
		printf ("keyname: %s, string: %s, relative: %s\n", keyName (curKey), keyString (curKey),
			elektraKeyGetRelativeName (curKey, parentKey));
	}
	ksDel (ksDbgDiff);
	ksDbgDiff = NULL;


	/* FIXME: Check what to do if some statements are not needed (e.g. only inserts, but no updates or deletions) */

	/* 3. Create and prepare the INSERT INTO, UPDATE and DELETE queries based on the given dsConfig */
	SQLHSTMT stmtDelete = NULL;
	SQLHSTMT stmtUpdate = NULL;
	SQLHSTMT stmtInsert = NULL;

	char * queryDeleteMeta = NULL;
	char * queryDelete = NULL;
	char * queryUpdate = NULL;
	char * queryInsert = NULL;

	KeySet * ksRemovedKeys = elektraDiffGetRemovedKeys (diffSet);
	KeySet * ksModifiedKeys = NULL;
	KeySet * ksInsertedKeys = NULL;


	bool succeeded = true;

	if (ksGetSize (ksRemovedKeys) > 0)
	{
		/* DELETE FROM table WHERE keyname IN (val1, val2, ...) */
		stmtDelete = getBoundDeleteStmt (sharedData->connection, ksRemovedKeys, parentKey);
		if (stmtDelete)
		{
			/* TODO: add configuration option for data sources that have cascading delete enabled
			 * --> no extra deletion of tuples in the metatable required */
			queryDeleteMeta = getDeleteQuery (ksRemovedKeys, sharedData->dsConfig->metaTableName,
							  sharedData->dsConfig->metaTableKeyColName, parentKey);
			queryDelete = getDeleteQuery (ksRemovedKeys, sharedData->dsConfig->tableName, sharedData->dsConfig->keyColName,
						      parentKey);
		}

		if (!queryDeleteMeta || !queryDelete)
		{
			succeeded = false;
		}
	}


	if (succeeded)
	{
		ksModifiedKeys = elektraDiffGetModifiedKeys (diffSet);

		if (ksGetSize (ksModifiedKeys) > 0)
		{
			stmtUpdate = getBoundUpdateStmt (sharedData->connection, sharedData->dsConfig, ksModifiedKeys, parentKey);

			if (stmtUpdate)
			{
				/* TODO: Call function for getting valid query string */
				queryUpdate = NULL;
			}

			if (!queryUpdate)
			{
				succeeded = false;
			}
		}
	}


	if (succeeded)
	{
		ksInsertedKeys = elektraDiffGetAddedKeys (diffSet);
		if (ksGetSize (ksInsertedKeys) > 0)
		{
			stmtInsert = getBoundInsertStmt (sharedData->connection, sharedData->dsConfig, ksInsertedKeys, parentKey);

			if (stmtInsert)
			{
				/* TODO: Call function for getting valid query string */
				queryInsert = NULL;
			}

			if (!queryInsert)
			{
				succeeded = false;
			}
		}
	}

	long ret = 0;
	if (succeeded)
	{
		/* Finally execute the statements (be aware that the change are only persisted when ending the active transaction in the
		 * commit phase of the set-operation. */
		long maxAffectedRows = -1;
		long curAffectedRows = -1;

		if (stmtDelete)
		{
			ELEKTRA_ASSERT (queryDeleteMeta && *queryDeleteMeta,
					"The DELETE query for the metatable was %s, but the SQL "
					"statement handle for the DELETE operation was not NULL!",
					queryDelete ? "empty" : "NULL");

			ELEKTRA_ASSERT (queryDelete && *queryDelete,
					"The DELETE query for the key-table was %s, but the SQL statement "
					"handle for the DELETE operation was not NULL!",
					queryDelete ? "empty" : "NULL");


			curAffectedRows = executeQuery (stmtDelete, queryDeleteMeta, parentKey);
			printf ("The DELETE query affected %ld row(s) in the metatable.\n", curAffectedRows);

			curAffectedRows = executeQuery (stmtDelete, queryDelete, parentKey);
			printf ("The DELETE query affected %ld row(s) in the key-table.\n", curAffectedRows);

			if (curAffectedRows > maxAffectedRows)
			{
				maxAffectedRows = curAffectedRows;
			}
		}

		if (stmtUpdate)
		{
			ELEKTRA_ASSERT (queryUpdate && *queryUpdate,
					"The UPDATE query was %s, but the SQL statement handle for the UPDATE operation "
					"was not NULL!",
					queryUpdate ? "empty" : "NULL");
			curAffectedRows = executeQuery (stmtUpdate, queryUpdate, parentKey);

			printf ("The UPDATE query affected %ld row(s).\n", curAffectedRows);

			if (curAffectedRows > maxAffectedRows)
			{
				maxAffectedRows = curAffectedRows;
			}
		}

		if (stmtInsert)
		{
			ELEKTRA_ASSERT (queryInsert && *queryInsert,
					"The INSERT query was %s, but the SQL statement handle for the INSERT operation "
					"was not NULL!",
					queryInsert ? "empty" : "NULL");
			curAffectedRows = executeQuery (stmtInsert, queryInsert, parentKey);

			printf ("The INSERT query affected %ld row(s).\n", curAffectedRows);

			if (curAffectedRows > maxAffectedRows)
			{
				maxAffectedRows = curAffectedRows;
			}
		}

		/* no disconnecting or freeing here, the handle to the connection and the SQL environment are needed until
		 * the transaction is committed or rolled back. */
		ELEKTRA_LOG_DEBUG ("Finished executing queries in open transaction, make sure to end the transaction.");
		ret = maxAffectedRows;
	}
	else
	{
		ret = -1;
	}

	/* As we've processed all queries, we now set the active environment and connection handle on the shared data
	 * this also applies to failed attempts, as the ROLLBACK phase is executed in that case */
	// sharedData->environment = sqlEnv;
	// sharedData->connection = sqlConnection;

	/* execute Query uses strings from keyNames in this KeySet --> can be deleted AFTER executeQuery() was called */
	ksDel (ksRemovedKeys);
	ksDel (ksModifiedKeys);
	ksDel (ksInsertedKeys);

	return ret;
}

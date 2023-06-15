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


/* This function creates a query string that can be used for deleting specific metakeys of a given key
 * If you want to delete all metadata for a key, you can use the getDeleteQuery() function */
static char * getDeleteMetaDataQuery (unsigned int numMetaKeys, const struct dataSourceConfig * dsConfig)
{
	if (!dsConfig || numMetaKeys == 0)
	{
		return NULL;
	}

	/* Example: DELETE FROM metaTable WHERE keyName=myKey AND metaKeyName IN (mk1, mk2, mk3) */
	size_t queryStrLen = strlen ("DELETE FROM ") + strlen (dsConfig->metaTableName) + strlen (" WHERE ") +
			     strlen (dsConfig->metaTableKeyColName) + strlen ("=? AND ") + strlen (dsConfig->metaTableMetaKeyColName) +
			     strlen (" IN ()");

	/* We need one comma less then number of metakeys to delete, so we already have to char for the \0 */
	queryStrLen += strlen ("?,") * numMetaKeys;

	char * queryStr = elektraMalloc (queryStrLen * sizeof (char));

	if (!queryStr)
	{
		return NULL;
	}

	char * strEnd = stpcpy (queryStr, "DELETE FROM ");
	strEnd = stpcpy (strEnd, dsConfig->metaTableName);
	strEnd = stpcpy (strEnd, " WHERE ");
	strEnd = stpcpy (strEnd, dsConfig->metaTableKeyColName);
	strEnd = stpcpy (strEnd, "=? AND ");
	strEnd = stpcpy (strEnd, dsConfig->metaTableMetaKeyColName);
	strEnd = stpcpy (strEnd, " IN (");

	for (unsigned int u = 0; u < numMetaKeys; u++)
	{
		if (u < (numMetaKeys - 1))
		{
			strEnd = stpcpy (strEnd, "?,");
		}
		else
		{
			/* last iteration */
			stpcpy (strEnd, "?)");
		}
	}

	return queryStr;
}


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
	if (numKeys == 0 || !tableName || !(*tableName) || !keyColName || !(*keyColName))
	{
		return NULL;
	}

	/* Example: DELETE from myTable WHERE keyName IN (key1, key2, key3) */
	size_t queryStrLen = strlen ("DELETE FROM ") + strlen (tableName) + strlen (" WHERE ") + strlen (keyColName) + strlen (" IN ()");

	/* One comma less than number of keys in the keyset is required, so we've already counted the char for \0 */
	queryStrLen += strlen ("?,") * numKeys;

	char * queryStr = elektraMalloc (queryStrLen * sizeof (char));

	if (!queryStr)
	{
		return NULL;
	}

	char * strEnd = stpcpy (queryStr, "DELETE FROM ");
	strEnd = stpcpy (strEnd, tableName);
	strEnd = stpcpy (strEnd, " WHERE ");
	strEnd = stpcpy (strEnd, keyColName);
	strEnd = stpcpy (strEnd, " IN (");

	for (unsigned int u = 0; u < numKeys; u++)
	{
		if (u < (numKeys - 1))
		{
			strEnd = stpcpy (strEnd, "?,");
		}
		else
		{
			/* last iteration */
			stpcpy (strEnd, "?)");
		}
	}
	return queryStr;
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
static bool bindStringsToStatementV (SQLHSTMT sqlStmt, ssize_t startPos, Key * errorKey, unsigned int numParams, va_list argList)
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

	if (startPos < 1)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (errorKey,
					      "The first parameter has the position 1, so the argument 'startPos' must be >= 1, "
					      "but you provided %zd",
					      startPos);
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
			ret = SQLBindParameter (sqlStmt, u + startPos, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR, 0, 0, NULL, 0,
						(SQLLEN *) SQL_NULL_DATA);
		}
		else
		{
			ret = SQLBindParameter (sqlStmt, u + startPos, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR, strlen (curStr), 0,
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

static bool bindStringsToStatement (SQLHSTMT sqlStmt, ssize_t startPos, Key * errorKey, unsigned int numParams, ...)
{
	va_list argList;
	va_start (argList, numParams);
	bool ret = bindStringsToStatementV (sqlStmt, startPos, errorKey, numParams, argList);
	va_end (argList);
	return ret;
}

/* Make sure to bind exactly the number of parameters that match the number of the placeholder which the query-string you want to execute
 * what the given statement handle has , startPos starts at 1*/
static bool bindKeyNamesToStatement (SQLHSTMT sqlStmt, const KeySet * keysToBind, ssize_t startPos, bool useRelativeKeyNames,
				     Key * parentKey)
{
	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided statement handle must not be NULL.");
		return false;
	}

	if (ksGetSize (keysToBind) < 1)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The KeySet with the keys to bind must not be NULL or empty.");
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

	if (startPos < 1)
	{
		ELEKTRA_SET_INTERFACE_ERRORF (parentKey,
					      "The first parameter has the position 1, so the argument 'startPos' must be >= 1, "
					      "but you provided %zd",
					      startPos);
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return false;
	}

	for (elektraCursor it = 0; it < ksGetSize (keysToBind); it++)
	{
		const char * curKeyName = NULL;
		if (useRelativeKeyNames)
		{
			curKeyName = elektraKeyGetRelativeName (ksAtCursor (keysToBind, it), parentKey);
		}
		else
		{
			/* use basename for metakeys to remove "meta:/" */
			curKeyName = keyBaseName (ksAtCursor (keysToBind, it));
		}

		SQLRETURN ret = SQLBindParameter (sqlStmt, it + startPos, SQL_PARAM_INPUT, SQL_C_CHAR, SQL_CHAR, strlen (curKeyName), 0,
						  (SQLCHAR *) curKeyName, (SQLLEN) strlen (curKeyName) + 1, NULL);

		printf ("Bound value '%s' to statement\n", curKeyName);

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


static SQLLEN removeRows (SQLHSTMT sqlStmt, const KeySet * ksRemovedKeys, const char * tableName, const char * keyColName, bool bindParams,
			  Key * parentKey)
{
	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided statement handle must not be NULL.");
		return -1;
	}

	if (bindParams && (!bindKeyNamesToStatement (sqlStmt, ksRemovedKeys, 1, true, parentKey)))
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


static SQLLEN processMetaDataForModifiedKey (SQLHSTMT sqlStmt, Key * modifiedKey, const struct dataSourceConfig * dsConfig,
					     const ElektraDiff * diffSet, Key * parentKey)
{
	/* For the UPDATE operation, we've to consider added, modified and deleted metadata
	 * --> this implies we may need INSERT, UPDATE and DELETE queries for the metadata */

	KeySet * ksMetaRemoved = elektraDiffGetRemovedMetaKeys (diffSet, modifiedKey);

	SQLLEN curAffectedRows;
	SQLLEN sumAffectedRows = 0;

	if (ksGetSize (ksMetaRemoved) > 0)
	{
		/* DELETE FROM metaTable where keyname=? and metaKeyName IN (?,?,?,...) */
		char * queryMetaDelete = getDeleteMetaDataQuery (ksGetSize (ksMetaRemoved), dsConfig);
		if (!queryMetaDelete)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			ksDel (ksMetaRemoved);
			return -1;
		}

		/* At first, we've to bind the key-name */
		bool ret = bindStringsToStatement (sqlStmt, 1, parentKey, 1, elektraKeyGetRelativeName (modifiedKey, parentKey));
		if (ret)
		{
			/* Now, we bind the names of the metakeys */
			ret = bindKeyNamesToStatement (sqlStmt, ksMetaRemoved, 2, false, parentKey);
		}

		if (!ret)
		{
			/* sqlStmt was already freed by the binding functions */
			ksDel (ksMetaRemoved);
			elektraFree (queryMetaDelete);
			return -1;
		}

		curAffectedRows = executeQuery (sqlStmt, queryMetaDelete, parentKey);
		elektraFree (queryMetaDelete);

		if (curAffectedRows == -1)
		{
			/* sqlStmt was already freed by executeQuery() */
			ksDel (ksMetaRemoved);
			return -1;
		}
		else if (curAffectedRows > 0)
		{
			sumAffectedRows += curAffectedRows;
		}
	}

	ksDel (ksMetaRemoved);


	KeySet * ksMetaAdded = elektraDiffGetAddedMetaKeys (diffSet, modifiedKey);

	if (ksGetSize (ksMetaAdded) > 0)
	{
		/* INSERT INTO metaTable (keyName, metaKeyName, metaValName) VALUES (?,?,?) */
		char * queryMetaInsert = getInsertQuery (dsConfig, true);

		if (!queryMetaInsert)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			ksDel (ksMetaAdded);
			return -1;
		}

		for (elektraCursor itMeta = 0; itMeta < ksGetSize (ksMetaAdded); itMeta++)
		{
			Key * curMetaKey = ksAtCursor (ksMetaAdded, itMeta);

			if (!bindStringsToStatement (sqlStmt, 1, parentKey, 3, elektraKeyGetRelativeName (modifiedKey, parentKey),
						     keyBaseName (curMetaKey), keyString (curMetaKey)))
			{
				/* sqlStmt was already freed by the bindStringsToStatement() */
				ksDel (ksMetaAdded);
				elektraFree (queryMetaInsert);
				return -1;
			}

			curAffectedRows = executeQuery (sqlStmt, queryMetaInsert, parentKey);

			if (curAffectedRows == -1)
			{
				/* sqlStmt was already freed by executeQuery() */
				ksDel (ksMetaAdded);
				elektraFree (queryMetaInsert);
				return -1;
			}
			else if (curAffectedRows > 0)
			{
				sumAffectedRows += curAffectedRows;
			}
		}

		elektraFree (queryMetaInsert);
	}

	ksDel (ksMetaAdded);


	KeySet * ksMetaModified = elektraDiffGetModifiedMetaKeys (diffSet, modifiedKey);

	if (ksGetSize (ksMetaModified) > 0)
	{
		/* UPDATE metaTable SET metaval=? where keyName=? and metaKeyName=? */
		char * queryMetaUpdate = getUpdateQuery (dsConfig, true);

		if (!queryMetaUpdate)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
			SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
			ksDel (ksMetaModified);
			return -1;
		}

		KeySet * ksNewMeta = keyMeta (modifiedKey);
		for (elektraCursor itMeta = 0; itMeta < ksGetSize (ksMetaModified); itMeta++)
		{
			Key * curMetaKey = ksAtCursor (ksMetaModified, itMeta);

			/* The curMetaKey contains the old metavalue --> we have to get the metakey from the modified key */
			Key * newMetaKey = ksLookup (ksNewMeta, curMetaKey, KDB_O_NONE);


			if (!bindStringsToStatement (sqlStmt, 1, parentKey, 3, keyString (newMetaKey),
						     elektraKeyGetRelativeName (modifiedKey, parentKey), keyBaseName (newMetaKey)))
			{
				/* sqlStmt was already freed by the bindStringsToStatement() */
				ksDel (ksMetaModified);
				elektraFree (queryMetaUpdate);
				return -1;
			}

			curAffectedRows = executeQuery (sqlStmt, queryMetaUpdate, parentKey);

			if (curAffectedRows == -1)
			{
				/* sqlStmt was already freed by executeQuery() */
				ksDel (ksMetaModified);
				elektraFree (queryMetaUpdate);
				return -1;
			}
			else if (curAffectedRows > 0)
			{
				sumAffectedRows += curAffectedRows;
			}
		}

		elektraFree (queryMetaUpdate);
	}

	ksDel (ksMetaModified);

	return sumAffectedRows;
}


/* @p diffSet is only needed for UPDATE operation, for INSERT it is ignored */
static SQLLEN insertOrUpdateRows (SQLHSTMT sqlStmt, const KeySet * ks, const struct dataSourceConfig * dsConfig, bool update,
				  const ElektraDiff * diffSet, Key * parentKey)
{

	if (ksGetSize (ks) < 1)
	{
		/* no rows to insert */
		return 0;
	}

	if (!sqlStmt)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided statement handle must not be NULL.");
		return -1;
	}

	if (!dsConfig)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "The provided dataSourceConfig struct must not be NULL.");
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
		return -1;
	}

	if (update && !diffSet)
	{
		ELEKTRA_SET_INTERFACE_ERROR (parentKey, "For the UPDATE operation, the diffSet must not be NULL.");
		SQLFreeHandle (SQL_HANDLE_STMT, sqlStmt);
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
	SQLLEN sumAffectedRows = 0;

	printf ("INSERT/UPDATE before loop\n");

	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * curKey = ksAtCursor (ks, it);

		/* For the UPDATE operation, we have to check if the value was changed, it is possible that only metadata was changed */
		if (!update || elektraDiffKeyValueChanged (diffSet, curKey))
		{
			/* UPDATE table set val=? where key=?
			 * --> we have to bind the value first and then the key-name for which the value should be set
			 * The INSERT query was designed to have the value before the key-name, so that the same parameter order as in the
			 * UPDATE query can be used */
			if (!bindStringsToStatement (sqlStmt, 1, parentKey, 2, keyString (curKey),
						     elektraKeyGetRelativeName (curKey, parentKey)))
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


		/* Now process the metadata, this KeySet MUST NOT be deleted */
		KeySet * curMetaKs = keyMeta (curKey);


		if (update)
		{
			curAffectedRows = processMetaDataForModifiedKey (sqlStmt, curKey, dsConfig, diffSet, parentKey);
			if (curAffectedRows == -1)
			{
				elektraFree (query);
			}
			else if (curAffectedRows > 0)
			{
				sumAffectedRows += curAffectedRows;
			}
		}
		else if (ksGetSize (curMetaKs) > 0)
		{
			char * queryMetaInsert = getInsertQuery (dsConfig, true);

			for (elektraCursor itMeta = 0; itMeta < ksGetSize (curMetaKs); itMeta++)
			{
				Key * curMetaKey = ksAtCursor (curMetaKs, itMeta);

				/* INSERT metadata (3 parameters)
				INSERT INTO metaTableName (keyColName, metaKeyColName, valColName) VALUES (?,?,?) */
				if (!bindStringsToStatement (sqlStmt, 1, parentKey, 3, elektraKeyGetRelativeName (curKey, parentKey),
							     keyBaseName (curMetaKey), keyString (curMetaKey)))
				{
					elektraFree (query);
					elektraFree (queryMetaInsert);
					return -1;
				}

				/* Now insert the keys */
				curAffectedRows = executeQuery (sqlStmt, queryMetaInsert, parentKey);

				if (curAffectedRows == -1)
				{
					/* sqlStmt was already freed by executeQuery() */
					elektraFree (query);
					elektraFree (queryMetaInsert);
					return -1;
				}
				else if (curAffectedRows > 0)
				{
					sumAffectedRows += curAffectedRows;
				}
			}

			elektraFree (queryMetaInsert);
		}
	}

	printf ("INSERT/UPDATE after loop\n");

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


static KeySet * getModifiedNewKeys (KeySet * ks, KeySet * ksModifiedOldKeys)
{
	if (!ks || !ksModifiedOldKeys)
	{
		return NULL;
	}

	KeySet * ksModifiedNewKeys = ksNew (ksGetSize (ksModifiedOldKeys), KS_END);
	if (!ksModifiedNewKeys)
	{
		return NULL;
	}

	if (ksGetSize (ksModifiedOldKeys) == 0)
	{
		/* return empty KeySet */
		return ksModifiedNewKeys;
	}

	elektraCursor posCur = ksSearch (ks, ksAtCursor (ksModifiedOldKeys, 0));
	elektraCursor posEnd = ksSearch (ks, ksAtCursor (ksModifiedOldKeys, ksGetSize (ksModifiedOldKeys) - 1));

	ELEKTRA_ASSERT (posCur >= 0 && posEnd >= 0,
			"A key that is part of the KeySet with modified keys was not found in the KeySet with "
			"the keys that should be stored!");

	for (elektraCursor it = 0; it < ksGetSize (ksModifiedOldKeys); it++)
	{
		for (Key * curOldKey = ksAtCursor (ksModifiedOldKeys, it);
		     keyCmp (curOldKey, ksAtCursor (ks, posCur)) != 0 && posCur <= posEnd; posCur++)
			;
		/* no loop body */

		ELEKTRA_ASSERT (posCur <= posEnd,
				"A key that is part of the KeySet with modified keys was not found in the KeySet with "
				"the keys that should be stored!");

		ksAppendKey (ksModifiedNewKeys, ksAtCursor (ks, posCur));
	}


	ELEKTRA_ASSERT (ksGetSize (ksModifiedOldKeys) == ksGetSize (ksModifiedNewKeys),
			"The size of the KeySets for the modified "
			"keys with new and old values did not match!");

	return ksModifiedNewKeys;
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

		printf ("DELETE affected %ld rows!\n", curAffectedRows);

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

	/* The keys in ksModifiedKeys contain the old values, but we need the new values for storing the in the data source */
	KeySet * ksModifiedOldKeys = elektraDiffGetModifiedKeys (diffSet);

	if (ksGetSize (ksModifiedOldKeys) > 0)
	{
		KeySet * ksModifiedNewKeys = getModifiedNewKeys (ks, ksModifiedOldKeys);

		/* We only need the keys with the new values */
		ksDel (ksModifiedOldKeys);

		if (!ksModifiedNewKeys)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
			elektraDiffDel (diffSet);
			return -1;
		}

		if (!sqlStmt)
		{
			sqlStmt = allocateStatementHandle (sharedData->connection, parentKey);
			if (!sqlStmt)
			{
				ksDel (ksModifiedNewKeys);
				elektraDiffDel (diffSet);
				return -1;
			}
		}

		SQLLEN curAffectedRows = insertOrUpdateRows (sqlStmt, ksModifiedNewKeys, sharedData->dsConfig, true, diffSet, parentKey);
		ksDel (ksModifiedNewKeys);

		printf ("UPDATE affected %ld rows!\n", curAffectedRows);

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

		SQLLEN curAffectedRows = insertOrUpdateRows (sqlStmt, ksAddedKeys, sharedData->dsConfig, false, NULL, parentKey);
		ksDel (ksAddedKeys);

		printf ("INSERT affected %ld rows!\n", curAffectedRows);

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

	/* No disconnecting or freeing here, the handle to the connection and the SQL environment are needed until
	 * the transaction is committed or rolled back. */
	ELEKTRA_LOG_DEBUG ("Finished executing queries in open transaction, make sure to end the transaction.");
	return sumAffectedRows;
}

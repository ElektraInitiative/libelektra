/**
 * @file
 *
 * @brief Tests for the ODBC backend plugin
 *
 * These file does not contain tests that actually connect to an ODBC data source and therefore
 * also not for actually writing data to or reading data from a data source.
 *
 * Test for NULL, empty and invalid arguments are present.
 * Functions that don't require an active ODBC connection are also tested with valid input
 *
 * For testing the actual functionality of the ODBC backend and drivers you use, you can execute the 'benchmark_mountpoint'
 * for mountpoints with your ODBC data sources.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "backend_odbc_general.h"
#include "backend_odbc_get.h"
#include "backend_odbc_set.h"

#include <sqlext.h>
#include <tests_plugin.h>

void fillDsStructExceptDsName (struct dataSourceConfig * dsConfig)
{
	dsConfig->userName = "user";
	dsConfig->password = "pw";
	dsConfig->tableName = "table";
	dsConfig->keyColName = "key";
	dsConfig->valColName = "val";
	dsConfig->metaTableName = "meta";
	dsConfig->metaTableKeyColName = "key";
	dsConfig->metaTableMetaKeyColName = "metaKey";
	dsConfig->metaTableMetaValColName = "metaVal";
}


static void test_null_helpers (void)
{
	succeed_if (!extractOdbcErrors (SQL_HANDLE_ENV, NULL), "should return NULL");

	Key * testKey = keyNew ("/", KEY_END);

	succeed_if (-1 == setOdbcError (SQL_HANDLE_ENV, NULL, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), false, NULL),
		    "should return -1");
	succeed_if (-1 == setOdbcError (SQL_HANDLE_ENV, NULL, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), false, testKey),
		    "should return -1");
	succeed_if (-1 == setOdbcError (SQL_HANDLE_ENV, NULL, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), true, NULL),
		    "should return -1");
	succeed_if (-1 == setOdbcError (SQL_HANDLE_ENV, NULL, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), true, testKey),
		    "should return -1");

	succeed_if (!fillDsStructFromDefinitionKs (NULL, NULL), "should return NULL");
	succeed_if (!fillDsStructFromDefinitionKs (NULL, testKey), "should return NULL");

	succeed_if (!checkIdentifiersForSubString (NULL, NULL, NULL), "should return false");
	succeed_if (!checkIdentifiersForSubString (NULL, NULL, testKey), "should return false");
	succeed_if (!checkIdentifiersForSubString (NULL, "", NULL), "should return false");
	succeed_if (!checkIdentifiersForSubString (NULL, "", testKey), "should return false");
	succeed_if (!checkIdentifiersForSubString (NULL, "odbc", NULL), "should return false");
	succeed_if (!checkIdentifiersForSubString (NULL, "odbc", testKey), "should return false");

	succeed_if (!dsConfigToString (NULL), "should return NULL");

	succeed_if (!clearOdbcSharedData (NULL, false, false), "should return false");
	succeed_if (!clearOdbcSharedData (NULL, false, true), "should return false");
	succeed_if (!clearOdbcSharedData (NULL, true, false), "should return false");
	succeed_if (!clearOdbcSharedData (NULL, true, true), "should return false");

	succeed_if (!freeSharedHandles (NULL, NULL), "should return false");
	succeed_if (!freeSharedHandles (NULL, testKey), "should return false");

	keyDel (testKey);
}

static void test_null_general (void)
{
	Key * testKey = keyNew ("/", KEY_END);

	succeed_if (!allocateConnectionHandle (NULL, NULL), "should return NULL");
	succeed_if (!allocateConnectionHandle (NULL, testKey), "should return NULL");

	succeed_if (!allocateStatementHandle (NULL, NULL), "should return NULL");
	succeed_if (!allocateStatementHandle (NULL, testKey), "should return NULL");

	succeed_if (!setOdbcVersion (NULL, 0, NULL), "should return false");
	succeed_if (!setOdbcVersion (NULL, 0, testKey), "should return false");
	succeed_if (!setOdbcVersion (NULL, SQL_OV_ODBC2, testKey), "should return false");
	succeed_if (!setOdbcVersion (NULL, SQL_OV_ODBC3, testKey), "should return false");

	succeed_if (!setLoginTimeout (NULL, 0, NULL), "should return false");
	succeed_if (!setLoginTimeout (NULL, 0, testKey), "should return false");
	succeed_if (!setLoginTimeout (NULL, 1, NULL), "should return false");
	succeed_if (!setLoginTimeout (NULL, 1, testKey), "should return false");
	succeed_if (!setLoginTimeout (NULL, -1, NULL), "should return false");
	succeed_if (!setLoginTimeout (NULL, -1, testKey), "should return false");

	succeed_if (!setAutocommit (NULL, false, NULL), "should return false");
	succeed_if (!setAutocommit (NULL, false, testKey), "should return false");
	succeed_if (!setAutocommit (NULL, true, NULL), "should return false");
	succeed_if (!setAutocommit (NULL, true, testKey), "should return false");

	succeed_if (!endTransaction (NULL, false, NULL), "should return false");
	succeed_if (!endTransaction (NULL, false, testKey), "should return false");
	succeed_if (!endTransaction (NULL, true, NULL), "should return false");
	succeed_if (!endTransaction (NULL, true, testKey), "should return false");

	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	succeed_if (!connectToDataSource (NULL, NULL, NULL), "should return false");
	succeed_if (!connectToDataSource (NULL, NULL, testKey), "should return false");
	succeed_if (!connectToDataSource (NULL, dsConfig, NULL), "should return false");
	succeed_if (!connectToDataSource (NULL, dsConfig, testKey), "should return false");
	elektraFree (dsConfig);

	succeed_if (-1 == executeQuery (NULL, NULL, NULL), "should return -1");
	succeed_if (-1 == executeQuery (NULL, NULL, testKey), "should return -1");
	succeed_if (-1 == executeQuery (NULL, "", NULL), "should return -1");
	succeed_if (-1 == executeQuery (NULL, "", testKey), "should return -1");

	succeed_if (strcmp (getQuoteStr (NULL, NULL), "\"") == 0, "should return \"");
	succeed_if (strcmp (getQuoteStr (NULL, testKey), "\"") == 0, "should return \"");

	keyDel (testKey);
}

static void test_null_get (void)
{
	Key * testKey = keyNew ("/", KEY_END);
	succeed_if (!getKeysFromDataSource (NULL, false, NULL), "should return NULL");
	succeed_if (!getKeysFromDataSource (NULL, false, testKey), "should return NULL");
	succeed_if (!getKeysFromDataSource (NULL, true, NULL), "should return NULL");
	succeed_if (!getKeysFromDataSource (NULL, true, testKey), "should return NULL");
	keyDel (testKey);
}

static void test_null_set (void)
{
	KeySet * ks = ksNew (0, KS_END);
	Key * testKey = keyNew ("/", KEY_END);
	succeed_if (-1 == storeKeysInDataSource (NULL, NULL, NULL), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (NULL, NULL, testKey), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (NULL, ks, NULL), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (NULL, ks, testKey), "should return -1");
	keyDel (testKey);
	ksDel (ks);
}


static void test_invalid_helpers_OdbcError (void)
{
	SQLHENV hEnv = NULL;
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (hEnv, "Could not allocate SQL environment handle");

	succeed_if (!extractOdbcErrors (SQL_HANDLE_DBC, hEnv), "should return NULL");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!extractOdbcErrors (SQL_HANDLE_STMT, hEnv), "should return NULL");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!extractOdbcErrors (SQL_HANDLE_DESC, hEnv), "should return NULL");


	Key * testKey = keyNew ("/", KEY_END);
	succeed_if (-1 == setOdbcError (SQL_HANDLE_DBC, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), false, NULL),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (-1 == setOdbcError (SQL_HANDLE_DBC, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), false, testKey),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (-1 == setOdbcError (SQL_HANDLE_DBC, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), true, NULL),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (-1 == setOdbcError (SQL_HANDLE_DBC, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), true, testKey),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);

	succeed_if (-1 == setOdbcError (SQL_HANDLE_STMT, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), false, NULL),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (-1 == setOdbcError (SQL_HANDLE_STMT, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), false, testKey),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (-1 == setOdbcError (SQL_HANDLE_STMT, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), true, NULL),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (-1 == setOdbcError (SQL_HANDLE_STMT, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), true, testKey),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);

	succeed_if (-1 == setOdbcError (SQL_HANDLE_DESC, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), false, NULL),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (-1 == setOdbcError (SQL_HANDLE_DESC, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), false, testKey),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (-1 == setOdbcError (SQL_HANDLE_DESC, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), true, NULL),
		    "should return -1");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (-1 == setOdbcError (SQL_HANDLE_DESC, hEnv, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), true, testKey),
		    "should return -1");
}

static void test_invalid_helpers_fillDsStructFromDefinitionKs (void)
{
	Key * testKey = keyNew ("/", KEY_END);
	KeySet * ksWrongKeyName = ksNew (1, keyNew ("system:/dataSourceNameX", KEY_VALUE, "myDs", KEY_END), KS_END);
	succeed_if (!fillDsStructFromDefinitionKs (ksWrongKeyName, NULL), "should return NULL");
	succeed_if (!fillDsStructFromDefinitionKs (ksWrongKeyName, testKey), "should return NULL");
	ksDel (ksWrongKeyName);



	Key * keyDsName = keyNew ("system:/dataSourceName", KEY_VALUE, "dsName", KEY_END);
	Key * keyTableName = keyNew ("system:/table/name", KEY_VALUE, "tableName", KEY_END);
	Key * keyTKeyColName = keyNew ("system:/table/keyColName", KEY_VALUE, "tableKeyColName", KEY_END);
	Key * keyTValColName = keyNew ("system:/table/valColName", KEY_VALUE, "tableValColName", KEY_END);
	Key * keyMetaTableName = keyNew ("system:/metaTable/name", KEY_VALUE, "metaTableName", KEY_END);


	/* Test with missing meta table columns names */
	KeySet * ksMissingOrWrong = ksNew (9, keyDsName, keyTableName, keyTKeyColName, keyTValColName, keyMetaTableName, KS_END);
	succeed_if (!fillDsStructFromDefinitionKs (ksMissingOrWrong, NULL), "should return NULL");
	succeed_if (!fillDsStructFromDefinitionKs (ksMissingOrWrong, testKey), "should return NULL");


	Key * keyMtKeyColName = keyNew ("system:/metaTable/keyColName", KEY_VALUE, "mTableKeyColName", KEY_END);
	Key * keyMtMetaKeyColName = keyNew ("system:/metaTable/metaKeyColName", KEY_VALUE, "mTableMetaKeyColName", KEY_END);
	Key * keyMtMetaValColName = keyNew ("system:/metaTable/metaValColName", KEY_VALUE, "mTableMetaValColName", KEY_END);
	Key * keyTimeout = keyNew ("system:/timeout", KEY_VALUE, "odbc", KEY_END);

	/* Test with invalid timeout value */
	ksAppendKey (ksMissingOrWrong, keyMtKeyColName);
	ksAppendKey (ksMissingOrWrong, keyMtMetaKeyColName);
	ksAppendKey (ksMissingOrWrong, keyMtMetaValColName);
	ksAppendKey (ksMissingOrWrong, keyTimeout);

	struct dataSourceConfig * dsConfig = fillDsStructFromDefinitionKs (ksMissingOrWrong, NULL);
	succeed_if (dsConfig, "should return valid 'struct dataSourceConfig'");
	succeed_if (5 == dsConfig->timeOut, "A default timeout of 5 seconds should've been set.");
	elektraFree (dsConfig);

	keySetString (keyTimeout, "-1");
	ksAppendKey (ksMissingOrWrong, keyTimeout);
	dsConfig = fillDsStructFromDefinitionKs (ksMissingOrWrong, testKey);
	succeed_if (dsConfig, "should return valid 'struct dataSourceConfig'");
	succeed_if (5 == dsConfig->timeOut, "A default timeout of 5 seconds should've been set.");
	elektraFree (dsConfig);
	keyDel (testKey);

	ksDel (ksMissingOrWrong);
}

static void test_invalid_helpers (void)
{
	test_invalid_helpers_OdbcError();
	test_invalid_helpers_fillDsStructFromDefinitionKs();

	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	fillDsStructExceptDsName (dsConfig);
	succeed_if (!dsConfigToString (dsConfig), "should return NULL");
	elektraFree (dsConfig);
}

static void test_invalid_general (void)
{
	Key * testKey = keyNew ("/", KEY_END);
	SQLHENV hEnv = NULL;
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);

	succeed_if (hEnv, "Could not allocate SQL environment handle");

	/* Disabled because the tests fail on FreeBSD (setODBCVersion does return true) */
#ifndef __FreeBSD__
	/* pass invalid versions */
	succeed_if (!setOdbcVersion (hEnv, 0, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!setOdbcVersion (hEnv, 0, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!setOdbcVersion (hEnv, 999, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!setOdbcVersion (hEnv, 999, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
#endif

	/* pass invalid handle (connection instead of environment) */
	SQLHDBC hConn = NULL;
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	succeed_if (!hConn, "Could allocate SQL connection handle, despite no correct ODBC version was set before!");

	/* Now we set a correct ODBC version, afterward the allocation of the connection handle should succeed */
	succeed_if (setOdbcVersion (hEnv, SQL_OV_ODBC3, testKey), "should return true");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	succeed_if (hConn, "Could not allocate SQL connection handle");

	/* pass invalid handle (connection instead of environment) */
	succeed_if (!setOdbcVersion (hConn, SQL_OV_ODBC2, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	succeed_if (!setOdbcVersion (hConn, SQL_OV_ODBC2, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	succeed_if (!setOdbcVersion (hConn, SQL_OV_ODBC3, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	succeed_if (!setOdbcVersion (hConn, SQL_OV_ODBC3, testKey), "should return false");

	/* pass invalid handle (connection instead of statement) */
	succeed_if (-1 == executeQuery (hConn, NULL, NULL), "should return -1");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	succeed_if (-1 == executeQuery (hConn, NULL, testKey), "should return -1");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	succeed_if (-1 == executeQuery (hConn, "", NULL), "should return -1");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	succeed_if (-1 == executeQuery (hConn, "", testKey), "should return -1");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);

	/* pass invalid handle (connection instead of environment) */
	succeed_if (!allocateConnectionHandle (hConn, NULL), "should return NULL");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	succeed_if (!allocateConnectionHandle (hConn, testKey), "should return NULL");

	/* pass invalid handle (environment instead of connection) */
	succeed_if (!allocateStatementHandle (hEnv, NULL), "should return NULL");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!allocateStatementHandle (hEnv, testKey), "should return NULL");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);

	/* pass invalid handle (environment instead of connection) */
	succeed_if (!setLoginTimeout (hEnv, 0, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!setLoginTimeout (hEnv, 0, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);

	/* pass invalid handle (environment instead of connection) */
	succeed_if (!setAutocommit (hEnv, false, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!setAutocommit (hEnv, false, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!setAutocommit (hEnv, true, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!setAutocommit (hEnv, true, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);

	/* pass invalid handle (environment instead of connection) */
	succeed_if (!endTransaction (hEnv, false, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!endTransaction (hEnv, false, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!endTransaction (hEnv, true, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!endTransaction (hEnv, true, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);

	/* pass invalid handle (environment instead of connection) */
	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	succeed_if (!connectToDataSource (hEnv, NULL, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!connectToDataSource (hEnv, NULL, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!connectToDataSource (hEnv, dsConfig, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!connectToDataSource (hEnv, dsConfig, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	fillDsStructExceptDsName (dsConfig);
	dsConfig->dataSourceName = "dsName";
	succeed_if (!connectToDataSource (hEnv, dsConfig, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!connectToDataSource (hEnv, dsConfig, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	elektraFree (dsConfig);

	/* pass invalid handle (environment instead of connection) */
	succeed_if (strcmp(getQuoteStr (hEnv, NULL), "\"") == 0, "should return \"");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (strcmp(getQuoteStr (hEnv, testKey), "\"") == 0, "should return \"");



	/* Try to get statement handle from a non-active connection */
	hEnv = allocateEnvHandle (NULL);
	succeed_if (hEnv, "Could not allocate environment handle");
	setOdbcVersion (hEnv, SQL_OV_ODBC3, NULL);
	hConn = allocateConnectionHandle (hEnv, NULL);
	succeed_if (hConn, "Could not allocate connection handle");

	succeed_if (!allocateStatementHandle (hConn, NULL), "should return NULL (getting statement handle for "
		    "non-active connection)");

	/* Try to get quote string for non-active connection (should return default quote string) */
	hConn = allocateConnectionHandle (hEnv, NULL);
	succeed_if (hConn, "Could not allocate connection handle");
	succeed_if (strcmp(getQuoteStr (hConn, NULL), "\"") == 0, "should return \"");

	/* Try to end a transaction on a non-active connection */
	succeed_if (!endTransaction (hConn, false, NULL), "should return false");
	succeed_if (!endTransaction (hConn, false, testKey), "should return false");
	succeed_if (!endTransaction (hConn, true, NULL), "should return false");
	succeed_if (!endTransaction (hConn, true, testKey), "should return false");

	keyDel (testKey);
}

static void test_invalid_get (void)
{
	struct odbcSharedData * sharedData = elektraCalloc (sizeof (struct odbcSharedData));
	Key * testKey = keyNew ("/", KEY_END);

	succeed_if (!getKeysFromDataSource (sharedData, false, NULL), "should return NULL");
	succeed_if (!getKeysFromDataSource (sharedData, false, testKey), "should return NULL");

	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	fillDsStructExceptDsName (dsConfig);
	sharedData->dsConfig = dsConfig;

	succeed_if (!getKeysFromDataSource (sharedData, false, NULL), "should return NULL");
	succeed_if (!getKeysFromDataSource (sharedData, false, testKey), "should return NULL");

	keyDel (testKey);
	elektraFree (dsConfig);
	elektraFree (sharedData);
}

static void test_invalid_set (void)
{
	struct odbcSharedData * sharedData = elektraCalloc (sizeof (struct odbcSharedData));
	KeySet * testKs = ksNew (0, KS_END);
	Key * testKey = keyNew ("/", KEY_END);

	succeed_if (-1 == storeKeysInDataSource (sharedData, NULL, NULL), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (sharedData, NULL, testKey), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (sharedData, testKs, NULL), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (sharedData, testKs, testKey), "should return -1");

	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	fillDsStructExceptDsName (dsConfig);
	sharedData->dsConfig = dsConfig;

	succeed_if (-1 == storeKeysInDataSource (sharedData, NULL, NULL), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (sharedData, NULL, testKey), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (sharedData, testKs, NULL), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (sharedData, testKs, testKey), "should return -1");

	elektraFree (dsConfig);
	keyDel (testKey);
	ksDel (testKs);
	elektraFree (sharedData);
}


static void test_empty_helpers (void)
{
	KeySet * testKs = ksNew (0, KS_END);
	succeed_if (!fillDsStructFromDefinitionKs (testKs, NULL), "should return NULL");
	ksDel (testKs);


	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	Key * testKey = keyNew ("/", KEY_END);

	succeed_if (checkIdentifiersForSubString (dsConfig, NULL, NULL), "should return true");
	succeed_if (checkIdentifiersForSubString (dsConfig, NULL, testKey), "should return true");
	succeed_if (checkIdentifiersForSubString (dsConfig, "", NULL), "should return true");
	succeed_if (checkIdentifiersForSubString (dsConfig, "", testKey), "should return true");
	succeed_if (!checkIdentifiersForSubString (dsConfig, "odbc", NULL), "should return false");
	succeed_if (!checkIdentifiersForSubString (dsConfig, "odbc", testKey), "should return false");

	succeed_if (!dsConfigToString (dsConfig), "should return NULL");
	elektraFree (dsConfig);

	struct odbcSharedData * sharedData = elektraCalloc (sizeof (struct odbcSharedData));
	succeed_if (clearOdbcSharedData (sharedData, false, false), "should return true");
	succeed_if (clearOdbcSharedData (sharedData, false, true), "should return true");
	succeed_if (clearOdbcSharedData (sharedData, true, false), "should return true");
	succeed_if (clearOdbcSharedData (sharedData, true, true), "should return true");
	elektraFree (sharedData);

	Plugin * plugin = elektraCalloc (sizeof (Plugin));
	succeed_if (freeSharedHandles (plugin, NULL), "should return true");
	succeed_if (freeSharedHandles (plugin, testKey), "should return true");
	elektraFree (plugin);

	keyDel (testKey);
}

static void test_empty_general (void)
{
	SQLHENV sqlEnv = allocateEnvHandle (NULL);
	succeed_if (sqlEnv, "Could not allocate SQL environment handle!");


	succeed_if (setOdbcVersion (sqlEnv, SQL_OV_ODBC3, NULL), "should return true");
	SQLHDBC sqlConnection = allocateConnectionHandle (sqlEnv, NULL);
	succeed_if (sqlConnection, "Could not allocate SQL connection handle!");

	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	succeed_if (!connectToDataSource(sqlConnection, dsConfig, NULL), "should return false");
}

static void test_empty_get (void)
{
	struct odbcSharedData * sharedData = elektraCalloc (sizeof (struct odbcSharedData));
	Key * testKey = keyNew ("/", KEY_END);

	succeed_if (!getKeysFromDataSource (sharedData, false, NULL), "should return NULL");
	succeed_if (!getKeysFromDataSource (sharedData, false, testKey), "should return NULL");
	succeed_if (!getKeysFromDataSource (sharedData, true, NULL), "should return NULL");
	succeed_if (!getKeysFromDataSource (sharedData, true, testKey), "should return NULL");

	keyDel (testKey);
	elektraFree (sharedData);
}

static void test_empty_set (void)
{
	struct odbcSharedData * sharedData = elektraCalloc (sizeof (struct odbcSharedData));
	KeySet * ks = ksNew (0, KS_END);
	Key * testKey = keyNew ("/", KEY_END);
	succeed_if (-1 == storeKeysInDataSource (sharedData, NULL, NULL), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (sharedData, NULL, testKey), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (sharedData, ks, NULL), "should return -1");
	succeed_if (-1 == storeKeysInDataSource (sharedData, ks, testKey), "should return -1");
	keyDel (testKey);
	ksDel (ks);
	elektraFree (sharedData);
}


static void test_valid_helpers (void)
{
	Key * keyDsName = keyNew ("system:/dataSourceName", KEY_VALUE, "dsName", KEY_END);
	Key * keyTableName = keyNew ("system:/table/name", KEY_VALUE, "tableName", KEY_END);
	Key * keyTKeyColName = keyNew ("system:/table/keyColName", KEY_VALUE, "tableKeyColName", KEY_END);
	Key * keyTValColName = keyNew ("system:/table/valColName", KEY_VALUE, "tableValColName", KEY_END);
	Key * keyMetaTableName = keyNew ("system:/metaTable/name", KEY_VALUE, "metaTableName", KEY_END);
	Key * keyMtKeyColName = keyNew ("system:/metaTable/keyColName", KEY_VALUE, "mTableKeyColName", KEY_END);
	Key * keyMtMetaKeyColName = keyNew ("system:/metaTable/metaKeyColName", KEY_VALUE, "mTableMetaKeyColName", KEY_END);
	Key * keyMtMetaValColName = keyNew ("system:/metaTable/metaValColName", KEY_VALUE, "mTableMetaValColName", KEY_END);
	KeySet * ksValid = ksNew (9, keyDsName, keyTableName, keyTKeyColName, keyTValColName, keyMetaTableName, keyMtKeyColName,
				  keyMtMetaKeyColName, keyMtMetaValColName, KEY_END);

	struct dataSourceConfig * dsConfig = fillDsStructFromDefinitionKs (ksValid, NULL);
	succeed_if (dsConfig, "should return a valid 'struct dataSourceConfiguration");

	struct odbcSharedData sharedData;
	sharedData.connection = 0;
	sharedData.environment = 0;
	sharedData.dsConfig = dsConfig;
	clearOdbcSharedData (&sharedData, true, true);
	dsConfig = NULL;

	/* Check valid timeout values */
	Key * keyTimeout = keyNew ("system:/timeout", KEY_VALUE, "0", KEY_END);
	ksAppendKey (ksValid, keyTimeout);

	dsConfig = fillDsStructFromDefinitionKs (ksValid, NULL);
	succeed_if (dsConfig, "should return a valid 'struct dataSourceConfiguration");
	sharedData.dsConfig = dsConfig;
	clearOdbcSharedData (&sharedData, true, true);
	dsConfig = NULL;

	keySetString (keyTimeout, "127");
	ksAppendKey (ksValid, keyTimeout);

	dsConfig = fillDsStructFromDefinitionKs (ksValid, NULL);
	succeed_if (dsConfig, "should return a valid 'struct dataSourceConfiguration");

	char * dsConfigStr = dsConfigToString (dsConfig);
	succeed_if (dsConfigStr && *dsConfigStr, "should return non-empty string");
	elektraFree (dsConfigStr);
	dsConfigStr = NULL;

	succeed_if (!checkIdentifiersForSubString (dsConfig, "\"", NULL), "should return false");
	succeed_if (checkIdentifiersForSubString (dsConfig, "meta", NULL), "should return true");

	sharedData.dsConfig = dsConfig;
	clearOdbcSharedData (&sharedData, true, true);

	dsConfig = NULL;

}

static void test_valid_general (void)
{
	SQLHENV hEnv = allocateEnvHandle (NULL);
	succeed_if (hEnv, "Could not allocate SQL environment handle");

	succeed_if (setOdbcVersion (hEnv, SQL_OV_ODBC2, NULL), "should return true");
	succeed_if (setOdbcVersion (hEnv, SQL_OV_ODBC3, NULL), "should return true");
	succeed_if (setOdbcVersion (hEnv, SQL_OV_ODBC3_80, NULL), "should return true");

	SQLHDBC hConn = allocateConnectionHandle (hEnv, NULL);
	succeed_if (hConn, "Could not allocate SQL connection handle");

	succeed_if (setLoginTimeout (hConn, 0, NULL), "should return true");
	succeed_if (setLoginTimeout (hConn, 127, NULL), "should return true");

	succeed_if (setAutocommit (hConn, true, NULL), "should return true");
	succeed_if (setAutocommit (hConn, false, NULL), "should return true");

	SQLFreeHandle (SQL_HANDLE_DBC, hConn);
	SQLFreeHandle (SQL_HANDLE_ENV, hEnv);
}


static void test_null (void)
{
	test_null_helpers ();
	test_null_general ();
	test_null_get ();
	test_null_set ();
}

static void test_invalid (void)
{
	test_invalid_helpers ();
	test_invalid_general ();
	test_invalid_get ();
	test_invalid_set ();
}

static void test_empty (void)
{
	test_empty_helpers ();
	test_empty_general ();
	test_empty_get ();
	test_empty_set ();
}

static void test_valid (void)
{
	test_valid_helpers ();
	test_valid_general ();
}

int main (int argc, char ** argv)
{
	printf ("ODBC-BACKEND TESTS\n");
	printf ("==================\n\n");


	init (argc, argv);

	/* Test if functions return a value indicating an error if NULL-pointers are passed for required arguments */
	test_null ();

	/* Test if functions return a value indicating an error if invalid values are passed for required arguments */
	test_invalid ();

	/* Test if functions work as expected when passing empty values (e.g. empty structs or KeySets) */
	test_empty ();

	/* Test if functions work as expected when called with valid arguments */
	test_valid ();

	print_result ("testmod_backend_odbc");

	return nbError;
}

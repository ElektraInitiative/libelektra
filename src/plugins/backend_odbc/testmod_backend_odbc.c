/**
 * @file
 *
 * @brief Tests for the ODBC backend plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "backend_odbc_get.h"

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
	printf ("Test passing NULL arguments to ODBC backend *helper* functions\n");

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
	keyDel (testKey);

	succeed_if (!fillDsStructFromDefinitionKs (NULL, NULL), "should return NULL");

	succeed_if (!dsConfigToString (NULL), "should return NULL");
}

static void test_null_general (void)
{
	printf ("Test passing NULL arguments to ODBC backend *general* functions\n");

	Key * testKey = keyNew ("/", KEY_END);

	succeed_if (!setOdbcVersion (NULL, 0, NULL), "should return false");
	succeed_if (!setOdbcVersion (NULL, 0, testKey), "should return false");
	succeed_if (!setOdbcVersion (NULL, SQL_OV_ODBC2, testKey), "should return false");
	succeed_if (!setOdbcVersion (NULL, SQL_OV_ODBC3, testKey), "should return false");

	succeed_if (!allocateConnectionHandle (NULL, NULL), "should return NULL");
	succeed_if (!allocateConnectionHandle (NULL, testKey), "should return NULL");

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

	struct columnData * colData = elektraCalloc (sizeof (struct columnData));
	succeed_if (!bindColumns (NULL, NULL, NULL), "should return false");
	succeed_if (!bindColumns (NULL, NULL, testKey), "should return false");
	succeed_if (!bindColumns (NULL, colData, NULL), "should return false");
	succeed_if (!bindColumns (NULL, colData, testKey), "should return false");
	elektraFree (colData);

	keyDel (testKey);
}

static void test_null_get (void)
{
	printf ("Test passing NULL arguments to ODBC backend *get* functions\n");

	Key * testKey = keyNew ("/", KEY_END);
	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));

	succeed_if (!getKeysFromDataSource (NULL, NULL), "should return NULL");
	succeed_if (!getKeysFromDataSource (NULL, testKey), "should return NULL");
	succeed_if (!getKeysFromDataSource (dsConfig, NULL), "should return NULL");
	succeed_if (!getKeysFromDataSource (dsConfig, testKey), "should return NULL");

	elektraFree (dsConfig);
	keyDel (testKey);
}

static void test_invalid_helpers (void)
{
	SQLHENV hEnv = NULL;
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);

	succeed_if (hEnv, "Could not allocate SQL environment handle");
	succeed_if (!extractOdbcErrors (SQL_HANDLE_DBC, hEnv), "should return NULL");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!extractOdbcErrors (SQL_HANDLE_STMT, hEnv), "should return NULL");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!extractOdbcErrors (SQL_HANDLE_DESC, hEnv), "should return NULL");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);

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

	keyDel (testKey);

	KeySet * testKs = ksNew (0, KS_END);
	succeed_if (!fillDsStructFromDefinitionKs (testKs, NULL), "should return NULL");
	ksDel (testKs);


	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	succeed_if (!dsConfigToString (dsConfig), "should return NULL");
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

	/* pass invalid handle (connection instead of environment) */
	succeed_if (!allocateConnectionHandle (hConn, NULL), "should return NULL");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	succeed_if (!allocateConnectionHandle (hConn, testKey), "should return NULL");
	SQLAllocHandle (SQL_HANDLE_DBC, hEnv, &hConn);
	SQLFreeHandle (SQL_HANDLE_DBC, hConn);

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

	/* pass invalid handle (environment instead of statement) */
	struct columnData * colData = elektraCalloc (sizeof (struct columnData));
	succeed_if (!bindColumns (hEnv, NULL, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!bindColumns (hEnv, NULL, testKey), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!bindColumns (hEnv, colData, NULL), "should return false");
	SQLAllocHandle (SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	succeed_if (!bindColumns (hEnv, colData, testKey), "should return false");
	elektraFree (colData);

	keyDel (testKey);
}

static void test_invalid_get (void)
{
	struct dataSourceConfig * dsConfig = elektraCalloc (sizeof (struct dataSourceConfig));
	Key * testKey = keyNew ("/", KEY_END);

	succeed_if (!getKeysFromDataSource (dsConfig, NULL), "should return NULL");
	succeed_if (!getKeysFromDataSource (dsConfig, testKey), "should return NULL");
	fillDsStructExceptDsName (dsConfig);
	succeed_if (!getKeysFromDataSource (dsConfig, NULL), "should return NULL");
	succeed_if (!getKeysFromDataSource (dsConfig, testKey), "should return NULL");

	keyDel (testKey);
	elektraFree (dsConfig);
}

static void test_null (void)
{
	test_null_helpers ();
	test_null_general ();
	test_null_get ();
}

static void test_invalid (void)
{
	test_invalid_helpers ();
	test_invalid_general ();
	test_invalid_get ();
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

	print_result ("testmod_backend_odbc");

	return nbError;
}

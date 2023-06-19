/**
 * @file
 *
 * @brief Helper functions for working with ODBC data sources
 *
 * This file contains constants, structs and functions that are used by the ODBC backend plugin.
 * They are generally usefully when working with ODBC data sources.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_BACKEND_ODBC_HELPERS_H
#define ELEKTRA_BACKEND_ODBC_HELPERS_H

#include <kdb.h>
#include <kdbplugin.h>
#include <stdbool.h>

/* ODBC related includes */
#include <sql.h>

/* Define standard buffer sizes (longer values should be handled by dynamically allocating larger buffers!)
 * If you have special use cases with mainly very short or long values, you can alter these values here for optimization.
 */
#define KEYNAME_BUFFER_SIZE 63
#define METAKEYNAME_BUFFER_SIZE 31
#define KEYSTRING_BUFFER_SIZE 255
#define METASTRING_BUFFER_SIZE KEYSTRING_BUFFER_SIZE


/** @brief The configuration of an ODBC data source
 *
 * Username and password are not needed by all data sources or may are defined as part of the data source definition.
 * For unixODBC, data sources are usually defined in the file odbc.ini
 */
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
	unsigned char timeOut;
};


struct odbcSharedData
{
	struct dataSourceConfig * dsConfig;

	/* Must be stored in the plugin data, because it is shared between phases (store, commit, rollback) */
	SQLHENV environment;
	SQLHDBC connection;
};

/** @brief Buffers for exchanging data with an ODBC data source
 *
 * This struct defines the buffers that are used for retrieving values from the datasource or saving values in the datasource.
 */
struct columnData
{
	SQLCHAR bufferKeyName[KEYNAME_BUFFER_SIZE];
	SQLLEN nameLenInd;

	SQLCHAR bufferKeyStr[KEYSTRING_BUFFER_SIZE];
	SQLLEN strLenInd;

	SQLCHAR bufferMetaKeyName[METAKEYNAME_BUFFER_SIZE];
	SQLLEN metaNameLenInd;

	SQLCHAR bufferMetaKeyStr[METASTRING_BUFFER_SIZE];
	SQLLEN metaStrLenInd;
};

/* Helper functions */

/* Extract ODBC driver errors, make sure to free the returned strings and the string array itself. */
char ** extractOdbcErrors (SQLSMALLINT handleType, SQLHANDLE odbcHandle);

/* Set an ODBC error or add a warning to a key */
int setOdbcError (SQLSMALLINT handleType, SQLHANDLE handle, const char * fileName, const char * functionName, const char * lineNo,
		  bool isWarning, Key * errorKey);

/* Macros to automatically fill the parameters for 'fileName', 'functionName' and 'lineNo' */
#define ELEKTRA_SET_ODBC_ERROR(handleType, handle, errorKey)                                                                               \
	do                                                                                                                                 \
	{                                                                                                                                  \
		setOdbcError (handleType, handle, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), false, errorKey);                      \
	} while (0)
#define ELEKTRA_ADD_ODBC_WARNING(handleType, handle, warningKey)                                                                           \
	do                                                                                                                                 \
	{                                                                                                                                  \
		setOdbcError (handleType, handle, __FILE__, __func__, ELEKTRA_STRINGIFY (__LINE__), true, warningKey);                     \
	} while (0)

/* A list of available data source names, make sure to free the returned strings and the string array itself. */
char ** getAvailableDataSources (void);

/* Fill the datasource config based on the Keys in the definition */
struct dataSourceConfig * fillDsStructFromDefinitionKs (KeySet * ksDefinition, Key * errorKey);

/* Check no identifier in the data source configuration contains the given substring (useful for finding illegal characters like quotes) */
bool checkIdentifiersForSubString (const struct dataSourceConfig * dsConfig, const char * subStr, Key * errorKey);

/* Get a string representation of all members of a dataSourceConfig struct which define a data source */
char * dsConfigToString (const struct dataSourceConfig * dsConfig);

/* Close the connection and free handles for connection and environment, optionally also free the data source config struct and its strings
 */
bool clearOdbcSharedData (struct odbcSharedData * sharedData, bool freeDsConfig, bool freeDsConfigStrings);

/* Close the connection and free handles for connection and environment */
bool freeSharedHandles (Plugin * plugin, Key * errorKey);

#endif // ELEKTRA_BACKEND_ODBC_HELPERS_H

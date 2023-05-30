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
 * For unixODBC, data sources are usually defined in the file /etc/unixODBC/odbc.ini
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
int setOdbcError (SQLSMALLINT handleType, SQLHANDLE handle, char * functionName, bool isWarning, Key * errorKey);

/* A list of available data source names, make sure to free the returned strings and the string array itself. */
char ** getAvailableDataSources (void);

/* Fill the datasource config based on the Keys in the definition */
struct dataSourceConfig * fillDsStructFromDefinitionKs (KeySet * ksDefinition);

/* Get the sum of the number of characters from all strings in the dataSourceConfig struct */
char * dsConfigToString (struct dataSourceConfig * dsConfig);


#endif // ELEKTRA_BACKEND_ODBC_HELPERS_H

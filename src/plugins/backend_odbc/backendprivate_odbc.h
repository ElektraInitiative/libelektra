/**
 * @file
 *
 * @brief Header for backend_odbc plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_BACKENDPRIVATE_ODBC_H
#define ELEKTRA_BACKENDPRIVATE_ODBC_H

#include <kdbprivate.h>

/* ODBC related includes */
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>


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

#define KEYNAME_BUFFER_SIZE 63
#define METAKEYNAME_BUFFER_SIZE 31
#define KEYSTRING_BUFFER_SIZE 255
#define METASTRING_BUFFER_SIZE KEYSTRING_BUFFER_SIZE

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

/* Extract ODBC driver errors, make sure to free the returned strings and the string array itself! */
char ** extractOdbcErrors (char * fn, SQLHANDLE odbcHandle, SQLSMALLINT type);

/* Log error or warning with the ELEKTRA_LOG_* macros */
void logError (SQLSMALLINT handleType, SQLHANDLE handle, char * functionName, bool isInfo, Key * parentKey);

/* A list of available data source names, make sure to free the returned strings and the string array itself! */
char ** getAvailableDataSources (void);

/* make sure to free the returned string */
char * getStringFromBaseName (KeySet * searchKs, Key * lookupKey, const char * baseName, bool addBaseName);

/* Fill the datasource config based on keys in the KDB */
struct dataSourceConfig * fillDsStructFromDefintionKs (KeySet * ksDefinition);

/* Get the sum of the number of characters from all strings in the dataSourceConfig struct */
char * dsConfigToString (struct dataSourceConfig * dsConfig);


#endif // ELEKTRA_BACKENDPRIVATE_ODBC_H

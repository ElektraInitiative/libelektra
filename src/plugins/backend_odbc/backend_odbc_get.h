#ifndef ELEKTRA_BACKEND_ODBC_GET_H
#define ELEKTRA_BACKEND_ODBC_GET_H

#include "./backend_odbc_general.h"

/* ODBC related includes */
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>


char * getSelectQueryString (struct dataSourceConfig * dsConfig, char * quoteString);
SQLHSTMT prepareSelectStmt (SQLHDBC sqlConnection, struct dataSourceConfig * dsConfig, Key * parentKey);
bool executeSelect (SQLHSTMT sqlStmt, Key * parentKey);
bool getLongData (SQLHSTMT sqlStmt, SQLUSMALLINT colNumber, SQLSMALLINT targetType, char ** targetValue, SQLLEN bufferSize,
		  Key * parentKey);
KeySet * fetchResults (SQLHSTMT sqlStmt, struct columnData * buffers, Key * parentKey);
KeySet * getKeysFromDataSource (struct dataSourceConfig * dsConfig, Key * parentKey);

#endif // ELEKTRA_BACKEND_ODBC_GET_H

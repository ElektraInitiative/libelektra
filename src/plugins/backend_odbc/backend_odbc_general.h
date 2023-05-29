#ifndef ELEKTRA_BACKEND_ODBC_GENERAL_H
#define ELEKTRA_BACKEND_ODBC_GENERAL_H

#include "./backend_odbc_helpers.h"

SQLHENV allocateEnvHandle (Key * parentKey);
bool setOdbcVersion (SQLHENV sqlEnv, unsigned long version, Key * parentKey);
SQLHDBC allocateConnectionHandle (SQLHENV sqlEnv, Key * parentKey);
bool setLoginTimeout (SQLHDBC sqlConnection, unsigned long timeout, Key * parentKey);
bool setAutocommit (SQLHDBC sqlConnection, bool enableAutoCommit, Key * parentKey);
bool endTransaction (SQLHDBC sqlConnection, bool commit, Key * parentKey);
bool connectToDataSource (SQLHDBC sqlConnection, struct dataSourceConfig * dsConfig, Key * parentKey);
bool bindColumns (SQLHSTMT sqlStmt, struct columnData * buffers, Key * parentKey);

#endif // ELEKTRA_BACKEND_ODBC_GENERAL_H

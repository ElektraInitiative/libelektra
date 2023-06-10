/**
 * @file
 *
 * @brief Header file for general functions needed by the ODBC backend plugin.
 *
 * This is the place for functions that are needed for setting up the environment or are used by both, get- and set-operations.
 * Some examples are allocating various handles, setting general attributes, connecting the data sources, etc.
 * This file is intended esp. for ODBC related tasks.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_BACKEND_ODBC_GENERAL_H
#define ELEKTRA_BACKEND_ODBC_GENERAL_H

#include "./backend_odbc_helpers.h"

SQLHENV allocateEnvHandle (Key * parentKey);
bool setOdbcVersion (SQLHENV sqlEnv, unsigned long version, Key * parentKey);
SQLHDBC allocateConnectionHandle (SQLHENV sqlEnv, Key * parentKey);
bool setLoginTimeout (SQLHDBC sqlConnection, unsigned long timeout, Key * errorKey);
bool setAutocommit (SQLHDBC sqlConnection, bool enableAutoCommit, Key * errorKey);
bool endTransaction (SQLHDBC sqlConnection, bool commit, Key * errorKey);
bool connectToDataSource (SQLHDBC sqlConnection, struct dataSourceConfig * dsConfig, Key * errorKey);
bool bindColumns (SQLHSTMT sqlStmt, struct columnData * buffers, Key * errorKey);

#endif // ELEKTRA_BACKEND_ODBC_GENERAL_H

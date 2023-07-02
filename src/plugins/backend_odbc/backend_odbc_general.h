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

#include <stdint.h>

SQLHENV allocateEnvHandle (Key * errorKey);
SQLHDBC allocateConnectionHandle (SQLHENV sqlEnv, Key * errorKey);
SQLHSTMT allocateStatementHandle (SQLHDBC sqlConnection, Key * errorKey);
bool setOdbcVersion (SQLHENV sqlEnv, unsigned long version, Key * parentKey);
bool setLoginTimeout (SQLHDBC sqlConnection, uintptr_t timeout, Key * errorKey);
bool setAutocommit (SQLHDBC sqlConnection, bool enableAutoCommit, Key * errorKey);
bool endTransaction (SQLHDBC sqlConnection, bool commit, Key * errorKey);
bool connectToDataSource (SQLHDBC sqlConnection, const struct dataSourceConfig * dsConfig, Key * errorKey);
SQLLEN executeQuery (SQLHSTMT stmt, const char * query, Key * errorKey);
char * getQuoteStr (SQLHDBC sqlConnection, Key * errorKey);

#endif // ELEKTRA_BACKEND_ODBC_GENERAL_H

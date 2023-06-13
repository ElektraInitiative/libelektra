/**
 * @file
 *
 * @brief Functions for getting data from an ODBC data source.
 *
 * This file contains all functions that are especially needed for retrieving data from a data source.
 * This includes building a string for a SELECT query, preparing and executing select queries and fetching data.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_BACKEND_ODBC_GET_H
#define ELEKTRA_BACKEND_ODBC_GET_H

#include "./backend_odbc_general.h"

/* ODBC related includes */
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>

KeySet * getKeysFromDataSource (struct dataSourceConfig * dsConfig, Key * errorKey);

#endif // ELEKTRA_BACKEND_ODBC_GET_H

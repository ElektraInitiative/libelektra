/**
 * @file
 *
 * @brief Functions for writing data to an ODBC data source.
 *	Insert- and update update operations are supported.
 *
 * This file contains all functions that are especially needed for setting data on a data source.
 * This includes building strings for INSERT INTO, UPDATE and DELETE queries, preparing and executing queries and sending data.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_BACKEND_ODBC_SET_H
#define ELEKTRA_BACKEND_ODBC_SET_H

#include "./backend_odbc_helpers.h"
#include <kdbdiff.h>

long storeKeysInDataSource (struct odbcSharedData * sharedData, const ElektraDiff * diffSet, Key * parentKey);

#endif // ELEKTRA_BACKEND_ODBC_SET_H

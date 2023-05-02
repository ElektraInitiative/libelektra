/**
 * @file
 *
 * @brief Header for template plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_BACKENDPRIVATEODBC_H
#define ELEKTRA_BACKENDPRIVATEODBC_H

#include <kdbprivate.h>

/* ODBC related includes */
#include <sql.h>
#include <sqlext.h>
#include <sqltypes.h>


typedef struct _PluginList
{
	Plugin * plugin;
	struct _PluginList * next;
} PluginList;

typedef struct
{
	char * dataSourceName;

	struct
	{
		Plugin * resolver;
		PluginList * prestorage;
		Plugin * storage;
		PluginList * poststorage;
	} getPositions;
	struct
	{
		Plugin * resolver;
		PluginList * prestorage;
		Plugin * storage;
		PluginList * poststorage;
		PluginList * precommit;
		Plugin * commit;
		PluginList * postcommit;
		PluginList * prerollback;
		Plugin * rollback;
		PluginList * postrollback;
	} setPositions;
} BackendHandle;


/* Helper functions */

/* Extract ODBC driver errors, make sure to free the returned strings and the string array itself! */
char ** extractOdbcErrors (char * fn, SQLHANDLE odbcHandle, SQLSMALLINT type);
/* A list of available data source names, make sure to free the returned strings and the string array itself! */
char ** getAvailableDataSources (void);

#endif // ELEKTRA_BACKENDPRIVATEODBC_H

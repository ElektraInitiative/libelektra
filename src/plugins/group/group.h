/**
 * @file
 *
 * @brief Header for group plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_GROUP_H
#define ELEKTRA_PLUGIN_GROUP_H

#include <kdbplugin.h>

#define ID_MAX_CHARACTERS 11
#define GROUP_FIELDS 3

struct group
{
	char * gr_name;   /* Group name.	*/
	char * gr_passwd; /* Password.	*/
	char * gr_gid;    /* Group ID.	*/
	char ** gr_mem;   /* Member list.	*/
};

typedef enum
{
	NAME,
	GID,
} SortBy;


int elektraGroupGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraGroupSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (group);

#endif

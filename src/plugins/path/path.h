/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_PATH_H
#define ELEKTRA_PLUGIN_PATH_H

#ifndef _GNU_SOURCE
	#define _GNU_SOURCE        // For euidaccess
#endif

#include <unistd.h>

#include <kdberrors.h>
#include <kdbplugin.h>

#include <errno.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <pwd.h>
#include <grp.h>

int elektraPathOpen (Plugin * handle, Key * errorKey);

int elektraPathClose (Plugin * handle, Key * errorKey);

int elektraPathGet (Plugin * handle, KeySet * ks, Key * parentKey);

int elektraPathSet (Plugin * handle, KeySet * ks, Key * parentKey);

int elektraPathError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (path);

#define ERRORMSG_LENGTH 1000

#endif

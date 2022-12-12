/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_PATH_H
#define ELEKTRA_PLUGIN_PATH_H

#include <unistd.h>

#include <elektra/kdbplugin.h>
#include <kdberrors.h>

#include <errno.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>

#include <grp.h>
#include <pwd.h>

int elektraPathOpen (Plugin * handle, Key * errorKey);

int elektraPathClose (Plugin * handle, Key * errorKey);

int elektraPathGet (Plugin * handle, KeySet * ks, Key * parentKey);

int elektraPathSet (Plugin * handle, KeySet * ks, Key * parentKey);

int elektraPathError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT;

#define ERRORMSG_LENGTH 1000

#endif

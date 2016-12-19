/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_PATH_H
#define ELEKTRA_PLUGIN_PATH_H

#include <kdberrors.h>
#include <kdbplugin.h>

#include <errno.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

int elektraPathOpen (Plugin * handle, Key * errorKey);
int elektraPathClose (Plugin * handle, Key * errorKey);
int elektraPathGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraPathSet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraPathError (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (path);

#define ERRORMSG_LENGTH 1000

#endif

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_PLUGIN_PATH_H
#define ELEKTRA_PLUGIN_PATH_H

#include <kdbplugin.h>
#include <kdberrors.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>

int elektraPathOpen(Plugin *handle, Key *errorKey);
int elektraPathClose(Plugin *handle, Key *errorKey);
int elektraPathGet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraPathSet(Plugin *handle, KeySet *ks, Key *parentKey);
int elektraPathError(Plugin *handle, KeySet *ks, Key *parentKey);

Plugin *ELEKTRA_PLUGIN_EXPORT(path);

#define ERRORMSG_LENGTH 1000

#endif

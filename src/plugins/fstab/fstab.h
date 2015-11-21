/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef FSTAB_H
#define FSTAB_H

// without that the backend does not work at all
// so it will be checked and backend will be disabled
// if mntent is not available
#include <mntent.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>
#include <errno.h>

#include <kdbplugin.h>
#include <kdbextension.h>
#include <kdberrors.h>


int elektraFstabGet(Plugin *handle, KeySet *returned, Key *parentKey);
int elektraFstabSet(Plugin *handle, KeySet *ks, Key *parentKey);

#endif

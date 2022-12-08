/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef FSTAB_H
#define FSTAB_H

// without that the backend does not work at all
// so it will be checked and backend will be disabled
// if mntent is not available
#include <mntent.h>

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <kdberrors.h>
#include <elektra/kdbextension.h>
#include <elektra/kdbplugin.h>


int elektraFstabGet (Plugin * handle, KeySet * returned, Key * parentKey);
int elektraFstabSet (Plugin * handle, KeySet * ks, Key * parentKey);

#endif

/***************************************************************************
            fstab.c  -  Access the /etc/fstab file
                             -------------------
    begin                : Mon Dec 26 2004
    copyright            : (C) 2004 by Markus Raab
    email                : elektra@markus-raab.org
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This is a backend that takes /etc/fstab file as its backend storage.  *
 *   The kdbGet() method will parse /etc/fstab and generate a              *
 *   valid key tree. The kdbSet() method will take a KeySet with valid     *
 *   filesystem keys and print an equivalent regular fstab in stdout.      *
 *                                                                         *
 ***************************************************************************/

#ifndef FSTAB_H
#define FSTAB_H

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

// without that the backend does not work at all
// so it will be checked and backend will be disabled
// if mntent is not available
#include <mntent.h>

#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <dirent.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <unistd.h>

#include <kdbplugin.h>
#include <kdbextension.h>

int elektraFstabGet(Plugin *handle, KeySet *returned, Key *parentKey);
int elektraFstabSet(Plugin *handle, KeySet *ks, Key *parentKey);

#endif

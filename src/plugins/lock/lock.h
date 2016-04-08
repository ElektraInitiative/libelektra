/**
 * @file
 *
 * @brief Header for lock plugin
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_LOCK_H
#define ELEKTRA_PLUGIN_LOCK_H

#include <kdbplugin.h>

#define LOCK_FILE_POSTFIX ".lock"

int elektraLockGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLockSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (lock);

#endif

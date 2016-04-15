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

#include <fcntl.h>    /* For O_* constants */
#include <sys/stat.h> /* For mode constants */
#include <semaphore.h>
#include <errno.h>


#define READ_COUNT "/elektra_lock_read_count"
#define WRITE_COUNT "/elektra_lock_write_count"

#define READ_MUTEX "/elektra_lock_read_mutex"
#define WRITE_MUTEX "/elektra_lock_write_mutex"
#define TRYREAD_MUTEX "/elektra_lock_tryread_mutex"
#define ACCESS_MUTEX "/elektra_lock_access_mutex"

#define SEM_MUTEX "/elektra_lock_sem_mutex"

int elektraLockOpen (Plugin * handle, Key * errorKey);
int elektraLockClose (Plugin * handle, Key * errorKey);
int elektraLockGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraLockSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (lock);

#endif

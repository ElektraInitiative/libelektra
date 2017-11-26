/**
 * @file
 *
 * @brief Header for semlock plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#ifndef ELEKTRA_PLUGIN_SEMLOCK_H
#define ELEKTRA_PLUGIN_SEMLOCK_H

#include <kdbplugin.h>

#include <errno.h>
#include <fcntl.h> /* For O_* constants */
#include <semaphore.h>
#include <sys/stat.h> /* For mode constants */


#define READ_COUNT "/elektra_semlock_read_count"
#define WRITE_COUNT "/elektra_semlock_write_count"

#define READ_MUTEX "/elektra_semlock_read_mutex"
#define WRITE_MUTEX "/elektra_semlock_write_mutex"
#define TRYREAD_MUTEX "/elektra_semlock_tryread_mutex"
#define ACCESS_MUTEX "/elektra_semlock_access_mutex"

#define SEM_MUTEX "/elektra_semlock_sem_mutex"

int elektraSemlockOpen (Plugin * handle, Key * errorKey);
int elektraSemlockClose (Plugin * handle, Key * errorKey);
int elektraSemlockGet (Plugin * handle, KeySet * ks, Key * parentKey);
int elektraSemlockSet (Plugin * handle, KeySet * ks, Key * parentKey);

Plugin * ELEKTRA_PLUGIN_EXPORT (semlock);

#endif

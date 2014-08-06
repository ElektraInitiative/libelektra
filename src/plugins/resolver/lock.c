#include "lock.h"

#include <kdbprivate.h>
#include <kdberrors.h>

#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#define ERROR_SIZE 1024

/**
 * Locks file for exclusive read/write mode.
 *
 * This function will not block until all reader
 * and writer have left the file.
 * -> conflict with other cooperative process detected,
 *    but we were later (and lost)
 *
 * @exception 27 set if locking failed, most likely a conflict
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 */
int elektraLockFile (int fd, Key *parentKey)
{
	struct flock l;
	l.l_type = F_WRLCK; /*Do exclusive Lock*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	int ret = fcntl (fd, F_SETLK, &l);

	if (ret == -1)
	{
		if (errno == EAGAIN || errno == EACCES)
		{
			ELEKTRA_SET_ERROR (30, parentKey, "conflict because other process writes to configuration indicated by lock");
		}
		else
		{
			char buffer[ERROR_SIZE];
			strerror_r(errno, buffer, ERROR_SIZE);
			ELEKTRA_ADD_WARNING(27, parentKey, buffer);

			ELEKTRA_SET_ERROR (30, parentKey, "assuming conflict because of failed lock (warning 27 for strerror)");
		}
		return -1;
	}

	return ret;
}


/**
 * Unlocks file.
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 */
int elektraUnlockFile (int fd, Key *parentKey)
{
	struct flock l;
	l.l_type = F_UNLCK; /*Give Lock away*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	int ret = fcntl (fd, F_SETLK, &l);

	if (ret == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_ADD_WARNING(32, parentKey, buffer);
	}

	return ret;
}


/**
 * @brief Close a file
 *
 * @param fd the filedescriptor to close
 * @param parentKey the key to write warnings to
 */
void elektraCloseFile(int fd, Key *parentKey)
{
	if (close (fd) == -1)
	{
		char buffer[ERROR_SIZE];
		strerror_r(errno, buffer, ERROR_SIZE);
		ELEKTRA_ADD_WARNING (33, parentKey, buffer);
	}
}

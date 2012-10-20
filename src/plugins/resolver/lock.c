#include "resolver.h"

#include <fcntl.h>
#include <unistd.h>

/**
 * Locks file for exclusive write mode.
 *
 * This function will block until all reader
 * and writer have left the file.
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 */
int elektraWriteLock (int fd)
{
	struct flock l;
	int ret=0;
	l.l_type = F_WRLCK; /*Do exclusive Lock*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	ret = fcntl (fd, F_SETLKW, &l);
	return ret;
}

/**
 * Locks file for read mode.
 *
 * Other processes and threads are allowed to read the
 * file too simultaneous.
 *
 * @param fd is a valid filedescriptor
 * @return 0 on success
 * @return -1 on failure
 * @ingroup backendhelper
 */
int elektraReadLock (int fd)
{
	int ret=0;
	struct flock l;
	l.l_type = F_RDLCK; /*Do read Lock*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	ret = fcntl (fd, F_SETLKW, &l);
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
int elektraUnlock (int fd)
{
	int ret=0;
	struct flock l;
	l.l_type = F_UNLCK; /*Give Lock away*/
	l.l_start= 0;	/*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0;	/*Do it with whole file*/
	ret = fcntl (fd, F_SETLKW, &l);
	return ret;
}


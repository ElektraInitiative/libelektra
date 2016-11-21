/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include "resolver.h"

#include <kdbproposal.h>

#include "kdbos.h"

#include <stdlib.h>

#ifdef HAVE_CTYPE_H
#include <ctype.h>
#endif

/* Needs posix */
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <unistd.h>

#include <dirent.h>

#include <kdberrors.h>
#include <kdblogger.h>
#include <kdbmacros.h>

#ifdef ELEKTRA_LOCK_MUTEX
#include <pthread.h>
#endif

#ifdef ELEKTRA_LOCK_MUTEX
#if defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP)
static pthread_mutex_t elektraResolverMutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP;
#elif defined(PTHREAD_RECURSIVE_MUTEX_INITIALIZER)
static pthread_mutex_t elektraResolverMutex = PTHREAD_RECURSIVE_MUTEX_INITIALIZER;
#else
static pthread_mutex_t elektraResolverMutex;
static pthread_mutex_t elektraResolverInitMutex = PTHREAD_MUTEX_INITIALIZER;
static unsigned char elektraResolverMutexInitialized = 0;
#define ELEKTRA_RESOLVER_RECURSIVE_MUTEX_INITIALIZATION
#endif
#endif

static void resolverInit (resolverHandle * p, const char * path)
{
	p->fd = -1;
	p->mtime.tv_sec = 0;
	p->mtime.tv_nsec = 0;
	p->filemode = KDB_FILE_MODE;
	p->dirmode = KDB_FILE_MODE | KDB_DIR_MODE;
	p->removalNeeded = 0;
	p->isMissing = 0;
	p->timeFix = 1;

	p->filename = 0;
	p->dirname = 0;
	p->tempfile = 0;

	p->path = path;

	p->uid = 0;
	p->gid = 0;
}

static resolverHandle * elektraGetResolverHandle (Plugin * handle, Key * parentKey)
{
	resolverHandles * pks = elektraPluginGetData (handle);
	switch (keyGetNamespace (parentKey))
	{
	case KEY_NS_SPEC:
		return &pks->spec;
	case KEY_NS_DIR:
		return &pks->dir;
	case KEY_NS_USER:
		return &pks->user;
	case KEY_NS_SYSTEM:
		return &pks->system;
	case KEY_NS_PROC:
	case KEY_NS_EMPTY:
	case KEY_NS_NONE:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
		return 0;
	}

	return 0;
}


static void resolverCloseOne (resolverHandle * p)
{
	elektraFree (p->filename);
	p->filename = 0;
	elektraFree (p->dirname);
	p->dirname = 0;
	elektraFree (p->tempfile);
	p->tempfile = 0;
}

static void resolverClose (resolverHandles * p)
{
	resolverCloseOne (&p->spec);
	resolverCloseOne (&p->dir);
	resolverCloseOne (&p->user);
	resolverCloseOne (&p->system);
	elektraFree (p);
}

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
 * @retval 0 on success
 * @retval -1 on failure
 * @ingroup backendhelper
 */
static int elektraLockFile (int fd ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
#ifdef ELEKTRA_LOCK_FILE
	struct flock l;
	l.l_type = F_WRLCK; /*Do exclusive Lock*/
	l.l_start = 0;      /*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0; /*Do it with whole file*/
	int ret = fcntl (fd, F_SETLK, &l);

	if (ret == -1)
	{
		if (errno == EAGAIN || errno == EACCES)
		{
			ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CONFLICT, parentKey,
					   "conflict because other process writes to configuration indicated by file lock");
		}
		else
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CONFLICT, parentKey,
					    "assuming conflict because of failed file lock with message: %s", strerror (errno));
		}
		return -1;
	}

	return ret;
#else
	return 0;
#endif
}


/**
 * Unlocks file.
 *
 * @param fd is a valid filedescriptor
 * @retval 0 on success
 * @retval -1 on failure
 * @ingroup backendhelper
 */
static int elektraUnlockFile (int fd ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
#ifdef ELEKTRA_LOCK_FILE
	struct flock l;
	l.l_type = F_UNLCK; /*Give Lock away*/
	l.l_start = 0;      /*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0; /*Do it with whole file*/
	int ret = fcntl (fd, F_SETLK, &l);

	if (ret == -1)
	{
		ELEKTRA_ADD_WARNINGF (32, parentKey, "fcntl SETLK unlocking failed with message: %s", strerror (errno));
	}

	return ret;
#else
	return 0;
#endif
}

/**
 * @brief mutex lock for multithread-safety
 *
 * @retval 0 on success
 * @retval -1 on error
 */
static int elektraLockMutex (Key * parentKey ELEKTRA_UNUSED)
{
#ifdef ELEKTRA_LOCK_MUTEX
	int ret = pthread_mutex_trylock (&elektraResolverMutex);
	if (ret != 0)
	{
		if (errno == EBUSY       // for trylock
		    || errno == EDEADLK) // for error checking mutex, if enabled
		{
			ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CONFLICT, parentKey,
					   "conflict because other thread writes to configuration indicated by mutex lock");
		}
		else
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CONFLICT, parentKey,
					    "assuming conflict because of failed mutex lock with message: %s", strerror (errno));
		}
		return -1;
	}
	return 0;
#else
	return 0;
#endif
}

/**
 * @brief mutex unlock for multithread-safety
 *
 * @retval 0 on success
 * @retval -1 on error
 */
static int elektraUnlockMutex (Key * parentKey ELEKTRA_UNUSED)
{
#ifdef ELEKTRA_LOCK_MUTEX
	int ret = pthread_mutex_unlock (&elektraResolverMutex);
	if (ret != 0)
	{
		ELEKTRA_ADD_WARNINGF (32, parentKey, "mutex unlock failed with message: %s", strerror (errno));
		return -1;
	}
	return 0;
#else
	return 0;
#endif
}


/**
 * @brief Close a file
 *
 * @param fd the filedescriptor to close
 * @param parentKey the key to write warnings to
 */
static void elektraCloseFile (int fd, Key * parentKey)
{
	if (close (fd) == -1)
	{
		ELEKTRA_ADD_WARNINGF (33, parentKey, "close failed with message: %s", strerror (errno));
	}
}

/**
 * @brief Add error text received from strerror
 *
 * @param errorText should have at least ERROR_SIZE bytes in reserve
 */
static void elektraAddErrnoText (char * errorText)
{
	char buffer[ERROR_SIZE];
	if (errno == E2BIG)
	{
		strcpy (buffer, "could not find a / in the pathname");
	}
	else if (errno == EINVAL)
	{
		strcpy (buffer, "went up to root for creating directory");
	}
	else
	{
		strcpy (buffer, strerror (errno));
	}
	strncat (errorText, buffer, ERROR_SIZE - 2 - strlen (errorText));
}

static int needsMapping (Key * testKey, Key * errorKey)
{
	elektraNamespace ns = keyGetNamespace (errorKey);

	if (ns == KEY_NS_NONE) return 1;      // for unit tests
	if (ns == KEY_NS_EMPTY) return 1;     // for default backend
	if (ns == KEY_NS_CASCADING) return 1; // init all namespaces for cascading

	return ns == keyGetNamespace (testKey); // otherwise only init if same ns
}

static int mapFilesForNamespaces (resolverHandles * p, Key * errorKey)
{
	Key * testKey = keyNew ("", KEY_END);
	// switch is only present to forget no namespace and to get
	// a warning whenever a new namespace is present.
	// In fact its linear code executed:
	switch (KEY_NS_SPEC)
	{
	case KEY_NS_SPEC:
		keySetName (testKey, "spec");
		if (needsMapping (testKey, errorKey) && ELEKTRA_PLUGIN_FUNCTION (resolver, filename) (testKey, &p->spec, errorKey) == -1)
		{
			resolverClose (p);
			keyDel (testKey);
			ELEKTRA_SET_ERROR (35, errorKey, "Could not resolve spec key");
			return -1;
		}

	case KEY_NS_DIR:
		keySetName (testKey, "dir");
		if (needsMapping (testKey, errorKey) && ELEKTRA_PLUGIN_FUNCTION (resolver, filename) (testKey, &p->dir, errorKey) == -1)
		{
			resolverClose (p);
			keyDel (testKey);
			ELEKTRA_SET_ERROR (35, errorKey, "Could not resolve dir key");
			return -1;
		}

	case KEY_NS_USER:
		keySetName (testKey, "user");
		if (needsMapping (testKey, errorKey) && ELEKTRA_PLUGIN_FUNCTION (resolver, filename) (testKey, &p->user, errorKey) == -1)
		{
			resolverClose (p);
			keyDel (testKey);
			ELEKTRA_SET_ERRORF (35, errorKey, "Could not resolve user key with conf %s", ELEKTRA_VARIANT_USER);
			return -1;
		}

	case KEY_NS_SYSTEM:
		keySetName (testKey, "system");
		if (needsMapping (testKey, errorKey) && ELEKTRA_PLUGIN_FUNCTION (resolver, filename) (testKey, &p->system, errorKey) == -1)
		{
			resolverClose (p);
			keyDel (testKey);
			ELEKTRA_SET_ERRORF (35, errorKey, "Could not resolve system key with conf %s", ELEKTRA_VARIANT_SYSTEM);
			return -1;
		}

	case KEY_NS_PROC:
	case KEY_NS_EMPTY:
	case KEY_NS_NONE:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
		break;
	}
	keyDel (testKey);
	return 0;
}

int ELEKTRA_PLUGIN_FUNCTION (resolver, open) (Plugin * handle, Key * errorKey)
{
	KeySet * resolverConfig = elektraPluginGetConfig (handle);
	if (ksLookupByName (resolverConfig, "/module", 0)) return 0;
	const char * path = keyString (ksLookupByName (resolverConfig, "/path", 0));

	if (!path)
	{
		ELEKTRA_SET_ERROR (34, errorKey, "Could not find file configuration");
		return -1;
	}

	resolverHandles * p = elektraMalloc (sizeof (resolverHandles));
	resolverInit (&p->spec, path);
	resolverInit (&p->dir, path);
	resolverInit (&p->user, path);
	resolverInit (&p->system, path);

#if defined(ELEKTRA_RESOLVER_RECURSIVE_MUTEX_INITIALIZATION)
	// PTHREAD_RECURSIVE_MUTEX_INITIALIZER_NP is available in glibc only
	// so we use another mutex for the initialization of the recursive mutex,
	// since this section must be thread safe.
	pthread_mutex_lock (&elektraResolverInitMutex);
	if (!elektraResolverMutexInitialized)
	{
		pthread_mutexattr_t mutexAttr;
		int mutexError;

		if ((mutexError = pthread_mutexattr_init (&mutexAttr)) != 0)
		{
			ELEKTRA_SET_ERRORF (35, errorKey, "Could not initialize recursive mutex: pthread_mutexattr_init returned %d",
					    mutexError);
			pthread_mutex_unlock (&elektraResolverInitMutex);
			return -1;
		}
		if ((mutexError = pthread_mutexattr_settype (&mutexAttr, PTHREAD_MUTEX_RECURSIVE)) != 0)
		{
			ELEKTRA_SET_ERRORF (35, errorKey, "Could not initialize recursive mutex: pthread_mutexattr_settype returned %d",
					    mutexError);
			pthread_mutex_unlock (&elektraResolverInitMutex);
			return -1;
		}
		if ((mutexError = pthread_mutex_init (&elektraResolverMutex, &mutexAttr)) != 0)
		{
			ELEKTRA_SET_ERRORF (35, errorKey, "Could not initialize recursive mutex: pthread_mutex_init returned %d",
					    mutexError);
			pthread_mutex_unlock (&elektraResolverInitMutex);
			return -1;
		}
		elektraResolverMutexInitialized = 1;
	}
	pthread_mutex_unlock (&elektraResolverInitMutex);
#endif

	// system and spec files need to be world-readable, otherwise they are
	// useless
	p->system.filemode = 0644;
	p->system.dirmode = 0755;
	p->spec.filemode = 0644;
	p->spec.dirmode = 0755;

	int ret = mapFilesForNamespaces (p, errorKey);

	elektraPluginSetData (handle, p);

	return ret; /* success */
}

int ELEKTRA_PLUGIN_FUNCTION (resolver, close) (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	resolverHandles * ps = elektraPluginGetData (handle);

	if (ps)
	{
		resolverClose (ps);
		elektraPluginSetData (handle, 0);
	}

	return 0; /* success */
}


int ELEKTRA_PLUGIN_FUNCTION (resolver, get) (Plugin * handle, KeySet * returned, Key * parentKey)
{
	resolverHandle * pk = elektraGetResolverHandle (handle, parentKey);

	Key * root = keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME, KEY_END);

	if (keyRel (root, parentKey) >= 0)
	{
		keyDel (root);
		KeySet * info =
#include "contract.h"
			ksAppend (returned, info);
		ksDel (info);
		return 1;
	}
	keyDel (root);

	keySetString (parentKey, pk->filename);

	int errnoSave = errno;
	struct stat buf;

	ELEKTRA_LOG ("stat file %s", pk->filename);
	/* Start file IO with stat() */
	if (stat (pk->filename, &buf) == -1)
	{
		// no file, so storage has no job
		errno = errnoSave;
		pk->mtime.tv_sec = 0;  // no file, so no time
		pk->mtime.tv_nsec = 0; // no file, so no time
		pk->isMissing = 1;
		return 0;
	}
	else
	{
		// successful, remember mode, uid and gid
		pk->filemode = buf.st_mode;
		pk->gid = buf.st_gid;
		pk->uid = buf.st_uid;
		pk->isMissing = 0;
	}

	/* Check if update needed */
	if (pk->mtime.tv_sec == elektraStatSeconds (buf) && pk->mtime.tv_nsec == elektraStatNanoSeconds (buf))
	{
		// no update, so storage has no job
		errno = errnoSave;
		return 0;
	}

	pk->mtime.tv_sec = elektraStatSeconds (buf);
	pk->mtime.tv_nsec = elektraStatNanoSeconds (buf);

	errno = errnoSave;
	return 1;
}


/**
 * @brief Add identity received from getuid(), geteuid(), getgid() and getegid()
 *
 * @param errorText should have at least ERROR_SIZE bytes in reserve
 */
static void elektraAddIdentity (char * errorText)
{
	char buffer[ERROR_SIZE];
	snprintf (buffer, ERROR_SIZE - 2, "uid: %u, euid: %u, gid: %u, egid: %u", getuid (), geteuid (), getgid (), getegid ());
	strcat (errorText, buffer);
}


/**
 * @brief Open a file and yield an error on conflicts
 *
 * @param pk->filename will be used
 * @param parentKey to yield the error to
 *
 * @retval 0 on success (might be an error for creating a missing file)
 * @retval -1 on conflict
 */
static int elektraOpenFile (resolverHandle * pk, Key * parentKey)
{
	int flags = 0;

	if (pk->isMissing)
	{
		// it must be created newly, otherwise we have an conflict
		flags = O_RDWR | O_CREAT | O_EXCL;

		// only works when using NFSv3 or later on kernel 2.6 or later
		// TODO: add variant with linkat?
	}
	else
	{
		// file was there before, so opening should work!
		flags = O_RDWR;
	}

	errno = 0;
	pk->fd = open (pk->filename, flags, pk->filemode);

	if (!pk->isMissing)
	{
		if (errno == ENOENT)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CONFLICT, parentKey,
					    "The configuration file \"%s\" was there earlier, "
					    "now it is missing",
					    pk->filename);
			return -1;
		}
		else if (pk->fd == -1)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_COULD_NOT_OPEN, parentKey,
					    "Could not reopen configuration file \"%s\" for writing because %s", pk->filename,
					    strerror (errno));
			return -1;
		}
		// successfully reopened
	}
	else
	{
		if (pk->fd != -1)
		{
			// successfully created a file
			pk->removalNeeded = 1;
			return 0;
		}
		else if (errno == EEXIST)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CONFLICT, parentKey,
					    "No configuration file was there earlier, "
					    "now configuration file \"%s\" exists",
					    pk->filename);
			return -1;
		}

		// ignore errors for attempts to create a new file, we will try it again later
	}

	errno = 0;

	return 0;
}


/**
 * @brief Create a file and yield an error if it did not work
 *
 * @param pk->filename will be used
 * @param parentKey to yield the error to
 *
 * @retval 0 on success
 * @retval -1 on error
 */
static int elektraCreateFile (resolverHandle * pk, Key * parentKey)
{
	pk->fd = open (pk->filename, O_RDWR | O_CREAT, pk->filemode);

	if (pk->fd == -1)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_COULD_NOT_OPEN, parentKey, "Could not create configuration file \"%s\" because %s",
				    pk->filename, strerror (errno));
		return -1;
	}
	return 0;
}


/**
 * @brief Create pathname recursively.
 *
 * Try unless the whole path was
 * created or it is sure that it cannot be done.
 *
 * @param pathname The path to create.
 *
 * @retval 0 on success
 * @retval -1 on error + elektra error will be set
 */
static int elektraMkdirParents (resolverHandle * pk, const char * pathname, Key * parentKey)
{
	if (mkdir (pathname, pk->dirmode) == -1)
	{
		if (errno != ENOENT)
		{
			// hopeless, give it up
			goto error;
		}

		// last part of filename component (basename)
		char * p = strrchr (pathname, '/');

		/* nothing found */
		if (p == NULL)
		{
			// set any errno, corrected in
			// elektraAddErrnoText
			errno = E2BIG;
			goto error;
		}

		/* absolute path */
		if (p == pathname)
		{
			// set any errno, corrected in
			// elektraAddErrnoText
			errno = EINVAL;
			goto error;
		}

		/* Cut path at last /. */
		*p = 0;

		/* Now call ourselves recursively */
		if (elektraMkdirParents (pk, pathname, parentKey) == -1)
		{
			// do not yield an error, was already done
			// before
			*p = '/';
			return -1;
		}

		/* Restore path. */
		*p = '/';

		if (mkdir (pathname, pk->dirmode) == -1)
		{
			goto error;
		}
	}

	return 0;

error:
{
	char * errorText = elektraMalloc (strlen (pathname) + ERROR_SIZE * 2 + 60);
	strcpy (errorText, "Could not create directory \"");
	strcat (errorText, pathname);
	strcat (errorText, "\", because: \"");
	elektraAddErrnoText (errorText);
	strcat (errorText, "\" ");
	elektraAddIdentity (errorText);
	ELEKTRA_SET_ERROR (74, parentKey, errorText);
	elektraFree (errorText);
	return -1;
}
}

/**
 * @brief Check conflict for the current open file
 *
 * Does an fstat and checks if mtime are equal as they were 
 *
 * @param pk to get mtime and fd from
 * @param parentKey to write errors&warnings to
 *
 * @retval 0 success
 * @retval -1 error
 */
static int elektraCheckConflict (resolverHandle * pk, Key * parentKey)
{
	if (pk->isMissing)
	{
		// conflict already handled at file creation time, so just return successfully
		return 0;
	}

	struct stat buf;

	if (fstat (pk->fd, &buf) == -1)
	{
		char * errorText = elektraMalloc (strlen (pk->filename) + ERROR_SIZE * 2 + 60);
		strcpy (errorText, "Could not fstat to check for conflict \"");
		strcat (errorText, pk->filename);
		strcat (errorText, "\" ");
		strcat (errorText, "because stat said: \"");
		elektraAddErrnoText (errorText);
		strcat (errorText, "\" ");
		elektraAddIdentity (errorText);
		ELEKTRA_ADD_WARNING (29, parentKey, errorText);
		elektraFree (errorText);

		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_CONFLICT, parentKey, "assuming conflict because of failed stat (warning 29 for details)");
		return -1;
	}

	if (elektraStatSeconds (buf) != pk->mtime.tv_sec || elektraStatNanoSeconds (buf) != pk->mtime.tv_nsec)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_CONFLICT, parentKey,
				    "conflict, file modification time stamp %ld.%ld is different than our time stamp %ld.%ld, config file "
				    "name is \"%s\", "
				    "our identity is uid: %u, euid: %u, gid: %u, egid: %u",
				    elektraStatSeconds (buf), elektraStatNanoSeconds (buf), pk->mtime.tv_sec, pk->mtime.tv_nsec,
				    pk->filename, getuid (), geteuid (), getgid (), getegid ());
		return -1;
	}


	return 0;
}

/**
 * @brief Check if setting keyset is needed.
 *
 * It is not needed, if the keyset is empty.
 * The configuration file gets removed then.
 *
 * @param pk resolver information
 * @param parentKey parent
 *
 * @retval 0 if nothing to do
 * @retval 1 set will be needed
 */
static int elektraRemoveConfigurationFile (resolverHandle * pk, Key * parentKey)
{
	if (unlink (pk->filename) == -1)
	{
		ELEKTRA_SET_ERROR (28, parentKey, strerror (errno));
	}

	return 0;
}

/**
 * @brief Does everything needed before the storage plugin will be
 * invoked.
 *
 * @param pk resolver information
 * @param parentKey parent
 *
 * @retval 0 on success
 * @retval -1 on error
 */
static int elektraSetPrepare (resolverHandle * pk, Key * parentKey)
{
	pk->removalNeeded = 0;

	if (elektraOpenFile (pk, parentKey) == -1)
	{
		// file/none-file conflict OR error on previously existing file
		return -1;
	}

	if (pk->fd == -1)
	{
		// try creation of underlying directory
		elektraMkdirParents (pk, pk->dirname, parentKey);

		// now try to create file
		if (elektraCreateFile (pk, parentKey) == -1)
		{
			// no way to be successful
			return -1;
		}

		// the file was created by us, so we need to remove it
		// on error:
		pk->removalNeeded = 1;
	}

	if (elektraLockMutex (parentKey) != 0)
	{
		elektraCloseFile (pk->fd, parentKey);
		pk->fd = -1;
		return -1;
	}

	// now we have a file, so lock immediately
	if (elektraLockFile (pk->fd, parentKey) == -1)
	{
		elektraCloseFile (pk->fd, parentKey);
		elektraUnlockMutex (parentKey);
		pk->fd = -1;
		return -1;
	}

	if (elektraCheckConflict (pk, parentKey) == -1)
	{
		elektraUnlockFile (pk->fd, parentKey);
		elektraCloseFile (pk->fd, parentKey);
		elektraUnlockMutex (parentKey);
		pk->fd = -1;
		return -1;
	}

	return 0;
}

static void elektraModifyFileTime (resolverHandle * pk)
{
#ifdef HAVE_CLOCK_GETTIME
	// for linux let us calculate a new ns timestamp to use
	struct timespec ts;
	clock_gettime (CLOCK_MONOTONIC, &ts);

	if (ts.tv_sec == pk->mtime.tv_sec)
	{
		// for filesystems not supporting subseconds, make sure the second is changed, too
		pk->mtime.tv_sec += pk->timeFix;
		pk->timeFix *= -1; // toggle timefix
	}
	else
	{
		pk->mtime.tv_sec = ts.tv_sec;
	}

	if (ts.tv_nsec == pk->mtime.tv_nsec)
	{
		// also slightly change nsec (same direction as seconds):
		pk->mtime.tv_nsec += pk->timeFix;
	}
	else
	{
		pk->mtime.tv_nsec = ts.tv_nsec;
	}
#else
	// otherwise use simple time toggling schema of seconds
	pk->mtime.tv_sec += pk->timeFix;
	pk->timeFix *= -1;								    // toggle timefix
#endif
}


/* Update timestamp of old file to provoke conflicts in
 * stalling processes that might still wait with the old
 * filedescriptor */
static void elektraUpdateFileTime (resolverHandle * pk, Key * parentKey)
{
#ifdef HAVE_FUTIMENS
	const struct timespec times[2] = { pk->mtime,   // atime
					   pk->mtime }; // mtime

	if (futimens (pk->fd, times) == -1)
	{
		ELEKTRA_ADD_WARNINGF (99, parentKey, "Could not update time stamp of \"%s\", because %s", pk->filename, strerror (errno));
	}
#elif defined(HAVE_FUTIMES)
	const struct timeval times[2] = { { pk->mtime.tv_sec, pk->mtime.tv_nsec / 1000 },   // atime
					  { pk->mtime.tv_sec, pk->mtime.tv_nsec / 1000 } }; // mtime

	if (futimes (pk->fd, times) == -1)
	{
		ELEKTRA_ADD_WARNINGF (99, parentKey, "Could not update time stamp of \"%s\", because %s", pk->filename, strerror (errno));
	}
#else
#warning futimens/futimes not defined
#endif
}

/**
 * @brief Now commit the temporary file to be final
 *
 * @param pk
 * @param parentKey
 *
 * It will also reset pk->fd
 *
 * @retval 0 on success
 * @retval -1 on error
 */
static int elektraSetCommit (resolverHandle * pk, Key * parentKey)
{
	int ret = 0;

	int fd = open (pk->tempfile, O_RDWR);
	if (fd == -1)
	{
		ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_COULD_NOT_OPEN, parentKey,
				    "Could not open file again for changing properties of file because %s", strerror (errno));
		ret = -1;
	}

	elektraLockFile (fd, parentKey);

	if (rename (pk->tempfile, pk->filename) == -1)
	{
		ELEKTRA_SET_ERROR (31, parentKey, strerror (errno));
		ret = -1;
	}

	struct stat buf;
	if (fstat (fd, &buf) == -1)
	{
		ELEKTRA_ADD_WARNING (29, parentKey, strerror (errno));
	}
	else
	{
		if (!(pk->mtime.tv_sec == elektraStatSeconds (buf) && pk->mtime.tv_nsec == elektraStatNanoSeconds (buf)))
		{
			/* Update timestamp */
			pk->mtime.tv_sec = elektraStatSeconds (buf);
			pk->mtime.tv_nsec = elektraStatNanoSeconds (buf);
		}
		else
		{
			elektraModifyFileTime (pk);
			// update file visible in filesystem:
			int pfd = pk->fd;
			pk->fd = fd;
			elektraUpdateFileTime (pk, parentKey);
			pk->fd = pfd;

			/* @post
			   For timejump backwards or time not changed,
			   use time + 1ns
			   This is needed to fulfill the postcondition
			   that the timestamp changed at least slightly
			   and makes sure that all processes that stat()ed
			   the file will get a conflict. */
		}
	}

	elektraUpdateFileTime (pk, parentKey);

	// file is present now!
	pk->isMissing = 0;

	if (buf.st_mode != pk->filemode)
	{
		// change mode to what it was before
		fchmod (fd, pk->filemode);
	}
	if (buf.st_uid != pk->uid || buf.st_gid != pk->gid)
	{
		fchown (fd, pk->uid, pk->gid);
	}

	DIR * dirp = opendir (pk->dirname);
	// checking dirp not needed, fsync will have EBADF
	if (fsync (dirfd (dirp)) == -1)
	{
		ELEKTRA_ADD_WARNINGF (88, parentKey, "Could not sync directory \"%s\", because %s", pk->dirname, strerror (errno));
	}
	closedir (dirp);

	elektraUnlockFile (pk->fd, parentKey);
	elektraCloseFile (pk->fd, parentKey);
	elektraUnlockFile (fd, parentKey);
	elektraCloseFile (fd, parentKey);
	elektraUnlockMutex (parentKey);

	return ret;
}


int ELEKTRA_PLUGIN_FUNCTION (resolver, set) (Plugin * handle, KeySet * ks, Key * parentKey)
{
	resolverHandle * pk = elektraGetResolverHandle (handle, parentKey);

	int errnoSave = errno;
	int ret = 1;

	if (pk->fd == -1)
	{
		// no fd up to now, so we are in first phase

		// we operate on the tmp file
		keySetString (parentKey, pk->tempfile);

		if (ksGetSize (ks) == 0)
		{
			ret = 0;

			// remove file on commit
			pk->fd = -2;
		}
		else
		{
			// prepare phase
			if (elektraSetPrepare (pk, parentKey) == -1)
			{
				ret = -1;
			}
		}
	}
	else if (pk->fd == -2)
	{
		// we commit the removal of the configuration file.
		elektraRemoveConfigurationFile (pk, parentKey);

		// reset for the next time
		pk->fd = -1;
	}
	else
	{
		// now we do not operate on the temporary file anymore,
		// but on the real file
		keySetString (parentKey, pk->filename);

		/* we have an fd, so we are in second phase*/
		if (elektraSetCommit (pk, parentKey) == -1)
		{
			ret = -1;
		}

		errno = errnoSave; // maybe some temporary error happened

		// reset for next time
		pk->fd = -1;
	}

	errno = errnoSave; // maybe some temporary error happened

	return ret;
}

static void elektraUnlinkFile (char * filename, Key * parentKey)
{
	int errnoSave = errno;
	if (unlink (filename) == -1)
	{
		ELEKTRA_ADD_WARNINGF (36, parentKey, "the file \"%s\" because of \"%s\"", filename, strerror (errno));
		errno = errnoSave;
	}
}

int ELEKTRA_PLUGIN_FUNCTION (resolver, error) (Plugin * handle, KeySet * r ELEKTRA_UNUSED, Key * parentKey)
{
	resolverHandle * pk = elektraGetResolverHandle (handle, parentKey);

	if (pk->fd == -2)
	{ // removal aborted state (= empty keyset, but error)
		// reset for next time
		pk->fd = -1;
		return 0;
	}

	elektraUnlinkFile (pk->tempfile, parentKey);

	if (pk->fd > -1)
	{ // with fd
		elektraUnlockFile (pk->fd, parentKey);
		elektraCloseFile (pk->fd, parentKey);
		if (pk->removalNeeded == 1)
		{ // removal needed state (= resolver created file, but error)
			elektraUnlinkFile (pk->filename, parentKey);
		}
		elektraUnlockMutex (parentKey);
	}

	// reset for next time
	pk->fd = -1;

	return 0;
}


Plugin * ELEKTRA_PLUGIN_EXPORT (resolver)
{
	// clang-format off
	return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
		ELEKTRA_PLUGIN_OPEN,	&ELEKTRA_PLUGIN_FUNCTION(resolver, open),
		ELEKTRA_PLUGIN_CLOSE,	&ELEKTRA_PLUGIN_FUNCTION(resolver, close),
		ELEKTRA_PLUGIN_GET,	&ELEKTRA_PLUGIN_FUNCTION(resolver, get),
		ELEKTRA_PLUGIN_SET,	&ELEKTRA_PLUGIN_FUNCTION(resolver, set),
		ELEKTRA_PLUGIN_ERROR,	&ELEKTRA_PLUGIN_FUNCTION(resolver, error),
		ELEKTRA_PLUGIN_END);
}


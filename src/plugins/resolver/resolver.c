/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "resolver.h"

#include <elektra/kdbhelper.h>	// elektraStrDup
#include <elektra/kdbprivate.h> // KDB_CACHE_PREFIX
#include <kdbassert.h>
#include <kdbconfig.h>

#include <elektra/kdbos.h>

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

#include <elektra/kdbmacros.h>
#include <kdberrors.h>
#include <kdblogger.h>

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
	ELEKTRA_ASSERT (pks != NULL, "Unable to retrieve plugin data for handle %p with parentKey %s", (void *) handle,
			keyName (parentKey));

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
	case KEY_NS_NONE:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
	case KEY_NS_DEFAULT:
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
	// shared by all, freed at the end
	char * path = (char *) p->system.path;
	resolverCloseOne (&p->spec);
	resolverCloseOne (&p->dir);
	resolverCloseOne (&p->user);
	resolverCloseOne (&p->system);
	elektraFree (path);
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
	l.l_start = 0;	    /*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0; /*Do it with whole file*/
	int ret = fcntl (fd, F_SETLK, &l);

	if (ret == -1)
	{
		if (errno == EAGAIN || errno == EACCES)
		{
			ELEKTRA_SET_RESOURCE_ERROR (parentKey,
						    "Conflict because other process writes to configuration indicated by file lock");
		}
		else
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Assuming conflict because of failed file lock. Reason: %s",
						     strerror (errno));
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
	l.l_start = 0;	    /*Start at begin*/
	l.l_whence = SEEK_SET;
	l.l_len = 0; /*Do it with whole file*/
	int ret = fcntl (fd, F_SETLK, &l);

	if (ret == -1)
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Method 'fcntl' unlocking failed (SETLK). Reason: %s", strerror (errno));
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
		if (errno == EBUSY	 // for trylock
		    || errno == EDEADLK) // for error checking mutex, if enabled
		{
			ELEKTRA_SET_CONFLICTING_STATE_ERROR (
				parentKey, "Conflict because other thread writes to configuration indicated by mutex lock");
		}
		else
		{
			ELEKTRA_SET_CONFLICTING_STATE_ERRORF (parentKey, "Assuming conflict because of failed mutex lock. Reason: %s",
							      strerror (errno));
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
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Mutex unlock failed. Reason: %s", strerror (errno));
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
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Close file failed. Reason: %s", strerror (errno));
	}
}

/**
 * @brief Add error text received from strerror
 *
 * @param errorText should have at least ERROR_SIZE bytes in reserve
 */
static char * elektraAddErrnoText (void)
{
	if (errno == E2BIG)
	{
		return "could not find a / in the pathname";
	}
	else if (errno == EINVAL)
	{
		return "went up to root for creating directory";
	}
	else
	{
		return strerror (errno);
	}
#if defined(__GNUC__) && __GNUC__ >= 8 && !defined(__clang__)
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstringop-truncation"
#endif
#if defined(__GNUC__) && __GNUC__ >= 8 && !defined(__clang__)
#pragma GCC diagnostic pop
#endif
}

static int needsMapping (Key * testKey, Key * errorKey)
{
	elektraNamespace ns = keyGetNamespace (errorKey);

	if (ns == KEY_NS_NONE) return 1;      // for unit tests
	if (ns == KEY_NS_CASCADING) return 1; // init all namespaces for cascading

	return ns == keyGetNamespace (testKey); // otherwise only init if same ns
}

static int mapFilesForNamespaces (resolverHandles * p, Key * errorKey)
{
	Key * testKey = keyNew ("/", KEY_END);
	// switch is only present to forget no namespace and to get
	// a warning whenever a new namespace is present.
	// In fact its linear code executed:
	ElektraResolved * resolved = NULL;
	switch (KEY_NS_SPEC)
	{
	case KEY_NS_SPEC:
		keySetName (testKey, "spec:/");
		if (needsMapping (testKey, errorKey))
		{
			if ((resolved = ELEKTRA_PLUGIN_FUNCTION (filename) (KEY_NS_SPEC, (p->spec).path, ELEKTRA_RESOLVER_TEMPFILE_SAMEDIR,
									    errorKey)) == NULL)
			{
				resolverClose (p);
				keyDel (testKey);
				ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Could not resolve filename. Could not resolve spec key");
				return -1;
			}
			else
			{
				p->spec.tempfile = elektraStrDup (resolved->tmpFile);
				p->spec.filename = elektraStrDup (resolved->fullPath);
				p->spec.dirname = elektraStrDup (resolved->dirname);
				ELEKTRA_PLUGIN_FUNCTION (freeHandle) (resolved);
			}
		}
		// FALLTHROUGH

	case KEY_NS_DIR:
		keySetName (testKey, "dir:/");
		if (needsMapping (testKey, errorKey))
		{
			if ((resolved = ELEKTRA_PLUGIN_FUNCTION (filename) (KEY_NS_DIR, (p->dir).path, ELEKTRA_RESOLVER_TEMPFILE_SAMEDIR,
									    errorKey)) == NULL)
			{
				resolverClose (p);
				keyDel (testKey);
				ELEKTRA_SET_RESOURCE_ERROR (errorKey, "Could not resolve filename. Could not resolve dir key");
				return -1;
			}
			else
			{
				p->dir.tempfile = elektraStrDup (resolved->tmpFile);
				p->dir.filename = elektraStrDup (resolved->fullPath);
				p->dir.dirname = elektraStrDup (resolved->dirname);
				ELEKTRA_PLUGIN_FUNCTION (freeHandle) (resolved);
			}
		}
	// FALLTHROUGH
	case KEY_NS_USER:
		keySetName (testKey, "user:/");
		if (needsMapping (testKey, errorKey))
		{
			if ((resolved = ELEKTRA_PLUGIN_FUNCTION (filename) (KEY_NS_USER, (p->user).path, ELEKTRA_RESOLVER_TEMPFILE_SAMEDIR,
									    errorKey)) == NULL)
			{
				resolverClose (p);
				keyDel (testKey);
				ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not resolve user key with configuration %s",
							     ELEKTRA_VARIANT_USER);
				return -1;
			}
			else
			{
				p->user.tempfile = elektraStrDup (resolved->tmpFile);
				p->user.filename = elektraStrDup (resolved->fullPath);
				p->user.dirname = elektraStrDup (resolved->dirname);
				ELEKTRA_PLUGIN_FUNCTION (freeHandle) (resolved);
			}
		}
	// FALLTHROUGH
	case KEY_NS_SYSTEM:
		keySetName (testKey, "system:/");
		if (needsMapping (testKey, errorKey))
		{
			if ((resolved = ELEKTRA_PLUGIN_FUNCTION (filename) (KEY_NS_SYSTEM, (p->system).path,
									    ELEKTRA_RESOLVER_TEMPFILE_SAMEDIR, errorKey)) == NULL)
			{
				resolverClose (p);
				keyDel (testKey);
				ELEKTRA_SET_RESOURCE_ERRORF (errorKey, "Could not resolve system key with configuration %s",
							     ELEKTRA_VARIANT_SYSTEM);
				return -1;
			}
			else
			{
				p->system.tempfile = elektraStrDup (resolved->tmpFile);
				p->system.filename = elektraStrDup (resolved->fullPath);
				p->system.dirname = elektraStrDup (resolved->dirname);
				ELEKTRA_PLUGIN_FUNCTION (freeHandle) (resolved);
			}
		}
	// FALLTHROUGH
	case KEY_NS_PROC:
	case KEY_NS_NONE:
	case KEY_NS_META:
	case KEY_NS_CASCADING:
	case KEY_NS_DEFAULT:
		break;
	}
	keyDel (testKey);
	return 0;
}

/**
 * @brief Generate key name for the cache
 *
 * @param filename the name of the config file
 * @ret pointer to the generated key name
 */
static char * elektraCacheKeyName (char * filename)
{
	char * name = 0;
	size_t len = strlen (KDB_CACHE_PREFIX) + strlen ("/") + strlen (ELEKTRA_PLUGIN_NAME) + strlen (filename) + 1;
	name = elektraMalloc (len);
	name = strcpy (name, KDB_CACHE_PREFIX);
	name = strcat (name, "/");
	name = strcat (name, ELEKTRA_PLUGIN_NAME);
	name = strcat (name, filename);

	ELEKTRA_LOG_DEBUG ("persistent chid key: %s", name);
	return name;
}

static int initHandles (Plugin * handle, Key * parentKey)
{
	const char * path = elektraStrDup (keyString (parentKey));

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
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not initialize recursive mutex: pthread_mutexattr_init returned %d",
						     mutexError);
			pthread_mutex_unlock (&elektraResolverInitMutex);
			return -1;
		}
		if ((mutexError = pthread_mutexattr_settype (&mutexAttr, PTHREAD_MUTEX_RECURSIVE)) != 0)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (
				parentKey, "Could not initialize recursive mutex: pthread_mutexattr_settype returned %d", mutexError);
			pthread_mutex_unlock (&elektraResolverInitMutex);
			return -1;
		}
		if ((mutexError = pthread_mutex_init (&elektraResolverMutex, &mutexAttr)) != 0)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not initialize recursive mutex: pthread_mutex_init returned %d",
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

	int ret = mapFilesForNamespaces (p, parentKey);

	if (ret != -1)
	{
		elektraPluginSetData (handle, p);
	}

	return ret;
}


int ELEKTRA_PLUGIN_FUNCTION (open) (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	elektraPluginSetData (handle, NULL);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int ELEKTRA_PLUGIN_FUNCTION (close) (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	resolverHandles * ps = elektraPluginGetData (handle);

	if (ps)
	{
		resolverClose (ps);
		elektraPluginSetData (handle, 0);
	}

	return 0; /* success */
}


int ELEKTRA_PLUGIN_FUNCTION (get) (Plugin * handle, KeySet * returned, Key * parentKey)
{
	Key * root = keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME, KEY_END);

	if (keyCmp (root, parentKey) == 0 || keyIsBelow (root, parentKey) == 1)
	{
		keyDel (root);
		KeySet * info =
#include "contract.h"
			ksAppend (returned, info);
		ksDel (info);
		return 1;
	}
	keyDel (root);

	if (elektraPluginGetData (handle) == NULL)
	{
		if (initHandles (handle, parentKey) == ELEKTRA_PLUGIN_STATUS_ERROR)
		{
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}

	resolverHandle * pk = elektraGetResolverHandle (handle, parentKey);
	keySetString (parentKey, pk->filename);

	int errnoSave = errno;
	struct stat buf;

	ELEKTRA_LOG ("stat file %s", pk->filename);
	/* Start file IO with stat() */
	if (stat (pk->filename, &buf) == -1)
	{
		// no file, so storage has no job
		errno = errnoSave;
		pk->isMissing = 1;

		// no file, so no metadata:
		pk->mtime.tv_sec = 0;
		pk->mtime.tv_nsec = 0;
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
	if (pk->mtime.tv_sec == ELEKTRA_STAT_SECONDS (buf) && pk->mtime.tv_nsec == ELEKTRA_STAT_NANO_SECONDS (buf))
	{
		// no update, so storage has no job
		errno = errnoSave;
		return 0;
	}

	/* Check if cache update needed */
	KeySet * global;
	char * name = elektraCacheKeyName (pk->filename);


	if ((global = elektraPluginGetGlobalKeySet (handle)) != NULL && ELEKTRA_STAT_NANO_SECONDS (buf) != 0)
	{
		// TODO [new_backend]: implement cache
		/*
		ELEKTRA_LOG_DEBUG ("global-cache: check cache update needed?");
		Key * time = ksLookupByName (global, name, KDB_O_NONE);
		if (time && keyGetValueSize (time) == sizeof (struct timespec))
		{
			struct timespec cached;
			keyGetBinary (time, &cached, sizeof (struct timespec));
			if (cached.tv_sec == ELEKTRA_STAT_SECONDS (buf) && cached.tv_nsec == ELEKTRA_STAT_NANO_SECONDS (buf))
			{
				ELEKTRA_LOG_DEBUG ("global-cache: no update needed, everything is fine");
				ELEKTRA_LOG_DEBUG ("cached.tv_sec:\t%ld", cached.tv_sec);
				ELEKTRA_LOG_DEBUG ("cached.tv_nsec:\t%ld", cached.tv_nsec);
				ELEKTRA_LOG_DEBUG ("buf.tv_sec:\t%ld", ELEKTRA_STAT_SECONDS (buf));
				ELEKTRA_LOG_DEBUG ("buf.tv_nsec:\t%ld", ELEKTRA_STAT_NANO_SECONDS (buf));
				// update timestamp inside resolver
				pk->mtime.tv_sec = ELEKTRA_STAT_SECONDS (buf);
				pk->mtime.tv_nsec = ELEKTRA_STAT_NANO_SECONDS (buf);

				if (name) elektraFree (name);
				errno = errnoSave;
				return ELEKTRA_PLUGIN_STATUS_CACHE_HIT;
			}
		}
		*/
	}

	pk->mtime.tv_sec = ELEKTRA_STAT_SECONDS (buf);
	pk->mtime.tv_nsec = ELEKTRA_STAT_NANO_SECONDS (buf);

	/* Persist modification times for cache */
	if (global != NULL && ELEKTRA_STAT_NANO_SECONDS (buf) != 0)
	{
		ELEKTRA_LOG_DEBUG ("global-cache: adding file modification times");
		Key * time = keyNew (name, KEY_BINARY, KEY_SIZE, sizeof (struct timespec), KEY_VALUE, &(pk->mtime), KEY_END);
		ksAppendKey (global, time);
	}

	if (name) elektraFree (name);
	errno = errnoSave;
	return 1;
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
		ELEKTRA_LOG_DEBUG ("creating %s", pk->filename);
		// it must be created newly, otherwise we have an conflict
		flags = O_RDWR | O_CREAT | O_EXCL;

		// only works when using NFSv3 or later on kernel 2.6 or later
		// TODO: add variant with linkat?
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("opening %s", pk->filename);
		// file was there before, so opening should work!
		flags = O_RDWR;
	}

	errno = 0;
	pk->fd = open (pk->filename, flags, pk->filemode);

	if (!pk->isMissing)
	{
		if (errno == ENOENT)
		{
			ELEKTRA_SET_INTERNAL_ERRORF (parentKey,
						     "The configuration file '%s' was there earlier, "
						     "now it is missing",
						     pk->filename);
			return -1;
		}
		else if (pk->fd == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not reopen configuration file '%s' for writing. Reason: %s",
						     pk->filename, strerror (errno));
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
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey,
						     "No configuration file was there earlier. "
						     "Now configuration file '%s' exists",
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
	ELEKTRA_LOG_DEBUG ("creating %s", pk->filename);
	pk->fd = open (pk->filename, O_RDWR | O_CREAT, pk->filemode);

	if (pk->fd == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not create configuration file '%s'. Reason: %s", pk->filename,
					     strerror (errno));
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

error : {
	ELEKTRA_SET_RESOURCE_ERRORF (parentKey,
				     "Could not create directory '%s'. Reason: %s. Identity: uid: %u, euid: %u, gid: %u, egid: %u",
				     pathname, elektraAddErrnoText (), getuid (), geteuid (), getgid (), getegid ());
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
		ELEKTRA_ADD_RESOURCE_WARNINGF (
			parentKey,
			"Could not 'fstat' to check for conflict '%s'. Reason: %s. Identity: uid: %u, euid: %u, gid: %u, egid: %u",
			pk->filename, elektraAddErrnoText (), getuid (), geteuid (), getgid (), getegid ());

		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Assuming conflict because of failed stat (warning %s for details)",
					     ELEKTRA_ERROR_RESOURCE);
		return -1;
	}

	if (ELEKTRA_STAT_SECONDS (buf) != pk->mtime.tv_sec || ELEKTRA_STAT_NANO_SECONDS (buf) != pk->mtime.tv_nsec)
	{
		ELEKTRA_SET_CONFLICTING_STATE_ERRORF (
			parentKey,
			"Conflict, file modification time stamp '%ld.%ld' is different than our time stamp '%ld.%ld', config file "
			"name is '%s'. "
			"Our identity is uid: %u, euid: %u, gid: %u, egid: %u",
			ELEKTRA_STAT_SECONDS (buf), ELEKTRA_STAT_NANO_SECONDS (buf), pk->mtime.tv_sec, pk->mtime.tv_nsec, pk->filename,
			getuid (), geteuid (), getgid (), getegid ());
		return -1;
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
static void elektraUpdateFileTime (resolverHandle * pk, int fd, Key * parentKey)
{
#ifdef HAVE_FUTIMENS
	const struct timespec times[2] = { pk->mtime,	// atime
					   pk->mtime }; // mtime

	if (futimens (fd, times) == -1)
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Could not update time stamp of '%s'. Reason: %s",
					       fd == pk->fd ? pk->filename : pk->tempfile, strerror (errno));
	}
#elif defined(HAVE_FUTIMES)
	const struct timeval times[2] = { { pk->mtime.tv_sec, pk->mtime.tv_nsec / 1000 },   // atime
					  { pk->mtime.tv_sec, pk->mtime.tv_nsec / 1000 } }; // mtime

	if (futimes (fd, times) == -1)
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Could not update time stamp of \"%s\", because %s",
					       fd == pk->fd ? pk->filename : pk->tempfile, strerror (errno));
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

	ELEKTRA_LOG_DEBUG ("committing %s to %s", pk->tempfile, pk->filename);

	int fd = open (pk->tempfile, O_RDWR);
	if (fd == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not open file '%s' again for changing metadata. Reason: %s", pk->tempfile,
					     strerror (errno));
		ret = -1;
	}

	elektraLockFile (fd, parentKey);

	if (rename (pk->tempfile, pk->filename) == -1)
	{
		ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not rename file '%s'. Reason: %s", pk->tempfile, strerror (errno));
		ret = -1;
	}

	ELEKTRA_LOG_DEBUG ("old.tv_sec:\t%ld", pk->mtime.tv_sec);
	ELEKTRA_LOG_DEBUG ("old.tv_nsec:\t%ld", pk->mtime.tv_nsec);
	struct stat buf;
	if (fstat (fd, &buf) == -1)
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Failed to stat file '%s'. Reason: %s", pk->tempfile, strerror (errno));
	}
	else
	{
		if (!(pk->mtime.tv_sec == ELEKTRA_STAT_SECONDS (buf) && pk->mtime.tv_nsec == ELEKTRA_STAT_NANO_SECONDS (buf)))
		{
			/* Update timestamp */
			pk->mtime.tv_sec = ELEKTRA_STAT_SECONDS (buf);
			pk->mtime.tv_nsec = ELEKTRA_STAT_NANO_SECONDS (buf);
		}
		else
		{
			elektraModifyFileTime (pk);
			// update file visible in filesystem:
			elektraUpdateFileTime (pk, fd, parentKey);

			/* @post
			   For timejump backwards or time not changed,
			   use time + 1ns
			   This is needed to fulfill the postcondition
			   that the timestamp changed at least slightly
			   and makes sure that all processes that stat()ed
			   the file will get a conflict. */
		}
	}

	elektraUpdateFileTime (pk, pk->fd, parentKey);
	ELEKTRA_LOG_DEBUG ("new.tv_sec:\t%ld", pk->mtime.tv_sec);
	ELEKTRA_LOG_DEBUG ("new.tv_nsec:\t%ld", pk->mtime.tv_nsec);

	if (buf.st_mode != pk->filemode)
	{
		// change mode to what it was before
		if (fchmod (fd, pk->filemode) == -1)
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey,
						       "Could not change permissions of temporary file '%s' from '%o' to '%o'. Reason: %s",
						       pk->tempfile, buf.st_mode, pk->filemode, strerror (errno));
		}
	}

	if (!pk->isMissing && (buf.st_uid != pk->uid || buf.st_gid != pk->gid))
	{
		if (fchown (fd, pk->uid, pk->gid) == -1)
		{
			ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey,
						       "Could not change owner of temporary file '%s' from %d.%d to %d.%d. Reason: %s",
						       pk->tempfile, buf.st_uid, buf.st_gid, pk->uid, pk->gid, strerror (errno));
		}
	}

	// file is present now!
	pk->isMissing = 0;

	DIR * dirp = opendir (pk->dirname);
	// checking dirp not needed, fsync will have EBADF
	if (fsync (dirfd (dirp)) == -1)
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Could not sync directory '%s'. Reason: %s", pk->dirname, strerror (errno));
	}
	closedir (dirp);

	elektraUnlockFile (pk->fd, parentKey);
	elektraCloseFile (pk->fd, parentKey);
	elektraUnlockFile (fd, parentKey);
	elektraCloseFile (fd, parentKey);
	elektraUnlockMutex (parentKey);

	return ret;
}


int ELEKTRA_PLUGIN_FUNCTION (set) (Plugin * handle, KeySet * ks, Key * parentKey)
{
	resolverHandle * pk = elektraGetResolverHandle (handle, parentKey);

	int errnoSave = errno;
	int ret = 1;

	ELEKTRA_LOG ("entering resolver::set %d \"%s\"", pk->fd, pk->filename);
	if (pk->fd == -1)
	{
		// no fd up to now, so we are in first phase

		// we operate on the tmp file
		keySetString (parentKey, pk->tempfile);

		if (ksGetSize (ks) == 0)
		{
			ret = 0;

			ELEKTRA_LOG ("check if removal of the configuration file \"%s\" would work later", pk->filename);
			if (access (pk->dirname, W_OK | X_OK) == -1)
			{
				ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not remove file '%s'. Reason: %s", pk->filename,
							     strerror (errno));
				ret = -1;
			}

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
		ELEKTRA_LOG ("unlink configuration file \"%s\"", pk->filename);
		if (access (pk->filename, F_OK) == 0 && unlink (pk->filename) == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not remove file '%s'. Reason: %s", pk->filename, strerror (errno));
			ret = -1;
		}

		ELEKTRA_LOG ("unlink temp file \"%s\"", pk->tempfile);
		if (access (pk->tempfile, F_OK) == 0 && unlink (pk->tempfile) == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not remove file '%s'. Reason: %s", pk->filename, strerror (errno));
			ret = -1;
		}

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

		// reset for next time
		pk->fd = -1;
	}

	ELEKTRA_LOG ("leaving resolver::set %d \"%s\"", pk->fd, pk->filename);

	errno = errnoSave; // maybe some temporary error happened

	return ret;
}

static void elektraUnlinkFile (char * filename, Key * parentKey)
{
	ELEKTRA_LOG ("unlinking %s", filename);
	int errnoSave = errno;
	if (access (filename, F_OK) == 0 && unlink (filename) == -1)
	{
		ELEKTRA_ADD_RESOURCE_WARNINGF (parentKey, "Could not unlink the file '%s'. Reason: %s", filename, strerror (errno));
		errno = errnoSave;
	}
}

int ELEKTRA_PLUGIN_FUNCTION (error) (Plugin * handle, KeySet * r ELEKTRA_UNUSED, Key * parentKey)
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

int ELEKTRA_PLUGIN_FUNCTION (commit) (Plugin * handle, KeySet * returned, Key * parentKey)
{
	return ELEKTRA_PLUGIN_FUNCTION (set) (handle, returned, parentKey);
}


Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport(ELEKTRA_PLUGIN_NAME,
            ELEKTRA_PLUGIN_OPEN,	&ELEKTRA_PLUGIN_FUNCTION(open),
            ELEKTRA_PLUGIN_CLOSE,	&ELEKTRA_PLUGIN_FUNCTION(close),
            ELEKTRA_PLUGIN_GET,	&ELEKTRA_PLUGIN_FUNCTION(get),
            ELEKTRA_PLUGIN_SET,	&ELEKTRA_PLUGIN_FUNCTION(set),
            ELEKTRA_PLUGIN_ERROR,	&ELEKTRA_PLUGIN_FUNCTION(error),
            ELEKTRA_PLUGIN_COMMIT, &ELEKTRA_PLUGIN_FUNCTION (commit),
            ELEKTRA_PLUGIN_END);
}


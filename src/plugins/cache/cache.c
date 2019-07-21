/**
 * @file
 *
 * @brief Source for cache plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "cache.h"

#ifdef HAVE_KDBCONFIG_H
#include "kdbconfig.h"
#endif

#include <kdbassert.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <kdblogger.h>
#include <kdbmodule.h>
#include <kdbprivate.h>

#include <fcntl.h>     // access()
#include <stdio.h>     // rename(), sprintf()
#include <sys/stat.h>  // elektraMkdirParents
#include <sys/time.h>  // gettimeofday()
#include <sys/types.h> // elektraMkdirParents
#include <unistd.h>    // access()

#define KDB_CACHE_STORAGE "mmapstorage"
#define POSTFIX_SIZE 50

typedef struct _cacheHandle CacheHandle;

struct _cacheHandle
{
	KeySet * modules;
	Key * cachePath;
	Plugin * resolver;
	Plugin * cacheStorage;
};

static int resolveCacheDirectory (Plugin * handle, CacheHandle * ch, Key * errorKey)
{
	KeySet * resolverConfig = ksNew (5, keyNew ("user/path", KEY_VALUE, "/.cache/elektra", KEY_END), KS_END);
	ch->resolver = elektraPluginOpen (KDB_RESOLVER, ch->modules, resolverConfig, ch->cachePath);
	if (!ch->resolver)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Open of plugin returned unsuccessfully: %s", KDB_RESOLVER);
		elektraModulesClose (ch->modules, 0);
		ksDel (ch->modules);
		keyDel (ch->cachePath);
		elektraFree (ch);
		return -1;
	}
	ch->resolver->global = elektraPluginGetGlobalKeySet (handle);
	// resolve cache directory in user home
	ch->resolver->kdbGet (ch->resolver, 0, ch->cachePath);

	return 0;
}

static int loadCacheStoragePlugin (Plugin * handle, CacheHandle * ch, Key * errorKey)
{
	KeySet * mmapstorageConfig = ksNew (0, KS_END);
	ch->cacheStorage = elektraPluginOpen (KDB_CACHE_STORAGE, ch->modules, mmapstorageConfig, ch->cachePath);
	if (!ch->cacheStorage)
	{
		ELEKTRA_ADD_PLUGIN_MISBEHAVIOR_WARNINGF (errorKey, "Open of plugin returned unsuccessfully: %s", KDB_CACHE_STORAGE);
		elektraPluginClose (ch->resolver, 0);
		elektraModulesClose (ch->modules, 0);
		ksDel (ch->modules);
		keyDel (ch->cachePath);
		elektraFree (ch);
		return -1;
	}
	ch->cacheStorage->global = elektraPluginGetGlobalKeySet (handle);

	return 0;
}

static int elektraMkdirParents (const char * pathname)
{
	if (mkdir (pathname, KDB_FILE_MODE | KDB_DIR_MODE) == -1)
	{
		if (errno != ENOENT)
		{
			// hopeless, give it up
			return -1;
		}

		// last part of filename component (basename)
		char * p = strrchr (pathname, '/');

		/* nothing found */
		if (p == NULL)
		{
			// set any errno, corrected in
			// elektraAddErrnoText
			errno = E2BIG;
			return -1;
		}

		/* absolute path */
		if (p == pathname)
		{
			// set any errno, corrected in
			// elektraAddErrnoText
			errno = EINVAL;
			return -1;
		}

		/* Cut path at last /. */
		*p = 0;

		/* Now call ourselves recursively */
		if (elektraMkdirParents (pathname) == -1)
		{
			// do not yield an error, was already done
			// before
			*p = '/';
			return -1;
		}

		/* Restore path. */
		*p = '/';

		if (mkdir (pathname, KDB_FILE_MODE | KDB_DIR_MODE) == -1)
		{
			return -1;
		}
	}

	return 0;
}

static char * elektraStrConcat (const char * a, const char * b)
{
	size_t len = strlen (a) + strlen (b) + 1;
	char * ret = elektraMalloc (len);
	ret = strcpy (ret, a);
	ret = strcat (ret, b);
	return ret;
}

static char * elektraGenTempFilename (char * cacheFileName)
{
	char * tmpFile = NULL;
	size_t len = 0;
	size_t tmpFilenameSize = 0;

	size_t cacheFileNameSize = strlen (cacheFileName);
	tmpFilenameSize = cacheFileNameSize + POSTFIX_SIZE;
	tmpFile = elektraCalloc (tmpFilenameSize);
	len = snprintf (tmpFile, cacheFileNameSize + 1, "%s", cacheFileName);

	struct timeval tv;
	memset (&tv, 0, sizeof (struct timeval));
	gettimeofday (&tv, 0);
	snprintf (tmpFile + len, POSTFIX_SIZE - 1, ".%d:%ld." ELEKTRA_TIME_USEC_F ".tmp", getpid (), tv.tv_sec, tv.tv_usec);
	return tmpFile;
}

static char * kdbCacheFileName (CacheHandle * ch, Key * parentKey)
{
	char * cacheFileName = 0;
	const char * directory = keyString (ch->cachePath);
	const char * name = keyName (parentKey);
	const char * value = keyString (parentKey);
	ELEKTRA_LOG_DEBUG ("mountpoint name: %s", name);
	if (strlen (name) != 0)
	{
		cacheFileName = elektraStrConcat (directory, "/backend/");
		char * tmp = cacheFileName;
		cacheFileName = elektraStrConcat (cacheFileName, name);
		elektraFree (tmp);
	}
	else if (elektraStrCmp (value, "default") == 0)
	{
		cacheFileName = elektraStrConcat (directory, "/default/");
	}
	else
	{
		ELEKTRA_LOG_DEBUG ("mountpoint empty, invalid cache file name");
		ELEKTRA_ASSERT (0 != 0, "mountpoint empty, invalid cache file name");
	}
	ELEKTRA_LOG_DEBUG ("cache dir: %s", cacheFileName);

	if (cacheFileName)
	{
		if (access (cacheFileName, O_RDWR) != 0)
		{
			elektraMkdirParents (cacheFileName);
		}

		char * tmp = cacheFileName;
		if (keyGetMeta (parentKey, "cascading"))
		{
			cacheFileName = elektraStrConcat (cacheFileName, "/cache_cascading.mmap");
		}
		else
		{
			cacheFileName = elektraStrConcat (cacheFileName, "/cache.mmap");
		}
		elektraFree (tmp);
		ELEKTRA_LOG_DEBUG ("cache file: %s", cacheFileName);
	}

	return cacheFileName;
}

int elektraCacheOpen (Plugin * handle, Key * errorKey)
{
	// plugin initialization logic
	// this function is optional
	ELEKTRA_LOG_DEBUG ("cache open");
	CacheHandle * ch = elektraMalloc (sizeof (CacheHandle));

	ch->modules = ksNew (0, KS_END);
	elektraModulesInit (ch->modules, 0);
	ch->cachePath = keyNew ("user/elektracache", KEY_END);

	if (resolveCacheDirectory (handle, ch, errorKey) == -1) return ELEKTRA_PLUGIN_STATUS_ERROR;
	if (loadCacheStoragePlugin (handle, ch, errorKey) == -1) return ELEKTRA_PLUGIN_STATUS_ERROR;

	elektraPluginSetData (handle, ch);
	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCacheClose (Plugin * handle, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	CacheHandle * ch = elektraPluginGetData (handle);
	if (ch)
	{
		elektraPluginClose (ch->resolver, 0);
		elektraPluginClose (ch->cacheStorage, 0);

		elektraModulesClose (ch->modules, 0);
		ksDel (ch->modules);
		keyDel (ch->cachePath);

		elektraFree (ch);
		elektraPluginSetData (handle, 0);
	}

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraCacheGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/cache"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/cache", KEY_VALUE, "cache plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/cache/exports", KEY_END),
			       keyNew ("system/elektra/modules/cache/exports/open", KEY_FUNC, elektraCacheOpen, KEY_END),
			       keyNew ("system/elektra/modules/cache/exports/close", KEY_FUNC, elektraCacheClose, KEY_END),
			       keyNew ("system/elektra/modules/cache/exports/get", KEY_FUNC, elektraCacheGet, KEY_END),
			       keyNew ("system/elektra/modules/cache/exports/set", KEY_FUNC, elektraCacheSet, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/cache/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	CacheHandle * ch = elektraPluginGetData (handle);
	if (ch->cacheStorage->global == 0)
	{
		ch->cacheStorage->global = elektraPluginGetGlobalKeySet (handle);
	}

	// construct cache file name from parentKey (which stores the mountpoint from mountGetMountpoint)
	Key * cacheFile = keyDup (parentKey);
	char * cacheFileName = kdbCacheFileName (ch, cacheFile);
	ELEKTRA_ASSERT (cacheFileName != 0, "Could not construct cache file name.");
	ELEKTRA_LOG_DEBUG ("CACHE get cacheFileName: %s, parentKey: %s, %s", cacheFileName, keyName (parentKey), keyString (parentKey));

	// load cache from storage
	keySetString (cacheFile, cacheFileName);
	elektraFree (cacheFileName);
	if (ch->cacheStorage->kdbGet (ch->cacheStorage, returned, cacheFile) == ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		keyDel (cacheFile);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

	keyDel (cacheFile); // TODO: maybe propagate errors?
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

int elektraCacheSet (Plugin * handle, KeySet * returned, Key * parentKey)
{
	// set all keys
	// this function is optional
	CacheHandle * ch = elektraPluginGetData (handle);
	if (ch->cacheStorage->global == 0)
	{
		ch->cacheStorage->global = elektraPluginGetGlobalKeySet (handle);
	}

	if (elektraPluginGetGlobalKeySet (handle) == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE; // TODO: do we fail silently here?
	}

	// construct cache file name from parentKey (which stores the mountpoint from mountGetMountpoint)
	Key * cacheFile = keyDup (parentKey);
	char * cacheFileName = kdbCacheFileName (ch, cacheFile);
	ELEKTRA_ASSERT (cacheFileName != 0, "Could not construct cache file name.");
	ELEKTRA_LOG_DEBUG ("CACHE set cacheFileName: %s, parentKey: %s, %s", cacheFileName, keyName (parentKey), keyString (parentKey));

	char * tmpFile = elektraGenTempFilename (cacheFileName);
	ELEKTRA_ASSERT (tmpFile != 0, "Could not construct temp file name.");
	ELEKTRA_LOG_DEBUG ("tmpFile: %s", tmpFile);

	// write cache to temp file
	keySetString (cacheFile, tmpFile);
	if (ch->cacheStorage->kdbSet (ch->cacheStorage, returned, cacheFile) == ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		if (rename (tmpFile, cacheFileName) == -1)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Could not rename file. Reason: %s", strerror (errno));
			goto error;
		}

		elektraFree (cacheFileName);
		elektraFree (tmpFile);
		keyDel (cacheFile);
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}

error:
	elektraFree (cacheFileName);
	elektraFree (tmpFile);
	keyDel (cacheFile);
	return ELEKTRA_PLUGIN_STATUS_ERROR;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("cache",
		ELEKTRA_PLUGIN_OPEN,	&elektraCacheOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraCacheClose,
		ELEKTRA_PLUGIN_GET,	&elektraCacheGet,
		ELEKTRA_PLUGIN_SET,	&elektraCacheSet,
		ELEKTRA_PLUGIN_END);
}

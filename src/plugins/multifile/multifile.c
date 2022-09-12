/**
 * @file
 *
 * @brief Source for multifile plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "multifile.h"

#include <dirent.h>
#include <errno.h>
#include <fnmatch.h>
#include <glob.h>
#include <kdbconfig.h>
#include <kdbhelper.h>
#include <kdbinternal.h>
#include <kdbmacros.h>
#include <kdbmodule.h>
#include <kdbplugin.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include "../resolver/shared.h"
#include <kdbinvoke.h>

#define DEFAULT_RESOLVER "resolver"
#define DEFAULT_PATTERN "*"
#define DEFAULT_STORAGE "storage"

typedef enum
{
	MULTI_SETRESOLVER = 0,
	MULTI_SETSTORAGE,
	MULTI_COMMIT,
} SetPhases;

typedef enum
{
	MULTI_GETRESOLVER = 0,
	MULTI_GETSTORAGE,
} GetPhases;

typedef enum
{
	EMPTY = -2,
	ERROR = -1,
	NOUPDATE = 0,
	SUCCESS = 1,
	CACHE_HIT = 2,
} Codes;


typedef struct
{
	char * directory;
	char * originalPath;
	char * pattern;
	SetPhases setPhase;
	GetPhases getPhase;
	ElektraKeyset * modules;
	ElektraKeyset * childBackends;
	ElektraKeyset * childConfig;
	char * resolver;
	char * storage;
	unsigned short stayAlive;
	unsigned short hasDeleted;
} MultiConfig;

typedef struct
{
	char * filename;
	char * fullPath;
	char * parentString;
	char * tmpFilename;
	Plugin * resolver;
	Codes rcResolver;
	Plugin * storage;
	Codes rcStorage;
	ElektraKeyset * ks;
} SingleConfig;

static inline Codes rvToRc (int rc)
{
	switch (rc)
	{
	case -2:
		return EMPTY;
		break;
	case -1:
		return ERROR;
		break;
	case 0:
		return NOUPDATE;
		break;
	case 1:
		return SUCCESS;
		break;
	case 2:
		return CACHE_HIT;
		break;
	}
	return ERROR;
}

static int elektraResolveFilename (ElektraKey * parentKey, ElektraResolveTempfile tmpFile)
{
	int rc = 0;
	ElektraInvokeHandle * handle = elektraInvokeOpen ("resolver", 0, 0);
	if (!handle)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}
	ElektraResolved * resolved = NULL;
	typedef ElektraResolved * (*resolveFileFunc) (elektraNamespace, const char *, ElektraResolveTempfile, ElektraKey *);
	resolveFileFunc resolveFunc = *(resolveFileFunc *) elektraInvokeGetFunction (handle, "filename");

	if (!resolveFunc)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}

	typedef void (*freeHandleFunc) (ElektraResolved *);
	freeHandleFunc freeHandle = *(freeHandleFunc *) elektraInvokeGetFunction (handle, "freeHandle");

	if (!freeHandle)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}

	resolved = resolveFunc (elektraKeyGetNamespace (parentKey), elektraKeyString (parentKey), tmpFile, parentKey);

	if (!resolved)
	{
		rc = -1;
		goto RESOLVE_FAILED;
	}
	else
	{
		elektraKeySetString (parentKey, resolved->fullPath);
		freeHandle (resolved);
	}

	elektraInvokeClose (handle, 0);
	return rc;

RESOLVE_FAILED:
	ELEKTRA_LOG_DEBUG ("MULTIFILE: resolve failed!");
	elektraInvokeClose (handle, 0);
	return rc;
}

int elektraMultifileCheckFile (const char * filename)
{
	if (!filename) return -1;
	if (filename[0] == '/') return 0;
	return 1;
}

int elektraMultifileOpen (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return 1; // success
}

static void closeBackend (SingleConfig * s)
{
	if (!s) return;
	if (s->fullPath) elektraFree (s->fullPath);
	if (s->filename) elektraFree (s->filename);
	if (s->parentString) elektraFree (s->parentString);
	if (s->tmpFilename) elektraFree (s->tmpFilename);
	if (s->resolver) elektraPluginClose (s->resolver, NULL);
	if (s->storage) elektraPluginClose (s->storage, NULL);
	if (s->ks) elektraKeysetDel (s->ks);
	elektraFree (s);
	s = NULL;
}

static void closeBackends (ElektraKeyset * b)
{
	elektraKeysetRewind (b);
	ElektraKey * k;
	while ((k = elektraKeysetNext (b)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **) elektraKeyValue (k);
		// fprintf (stderr, "closing backend %s:(%s)\n", s->parentString, s->fullPath);
		closeBackend (s);
	}
}

int elektraMultifileClose (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc) return 1;
	closeBackends (mc->childBackends);
	if (mc->directory) elektraFree (mc->directory);
	if (mc->pattern) elektraFree (mc->pattern);
	if (mc->resolver) elektraFree (mc->resolver);
	if (mc->storage) elektraFree (mc->storage);
	if (mc->originalPath) elektraFree (mc->originalPath);
	elektraModulesClose (mc->modules, NULL);
	elektraKeysetDel (mc->modules);
	elektraKeysetDel (mc->childBackends);
	elektraKeysetDel (mc->childConfig);
	elektraFree (mc);
	elektraPluginSetData (handle, NULL);
	return 1; // success
}

static MultiConfig * initialize (Plugin * handle, ElektraKey * parentKey)
{

	ElektraKeyset * config = elektraPluginGetConfig (handle);
	ElektraKey * origPath = elektraKeysetLookupByName (config, "/path", 0);
	elektraKeySetString (parentKey, elektraKeyString (origPath));
	if (elektraResolveFilename (parentKey, ELEKTRA_RESOLVER_TEMPFILE_NONE) == -1)
	{
		return NULL;
	}
	ElektraKey * patternKey = elektraKeysetLookupByName (config, "/pattern", 0);
	ElektraKey * storageKey = elektraKeysetLookupByName (config, "/storage", 0);
	ElektraKey * resolverKey = elektraKeysetLookupByName (config, "/resolver", 0);
	ElektraKey * stayAliveKey = elektraKeysetLookupByName (config, "/stayalive", 0);
	MultiConfig * mc = elektraCalloc (sizeof (MultiConfig));
	mc->directory = elektraStrDup (elektraKeyString (parentKey));
	mc->originalPath = elektraStrDup (elektraKeyString (origPath));
	if (resolverKey)
	{
		mc->resolver = elektraStrDup (elektraKeyString (resolverKey));
	}
	else
	{
		mc->resolver = elektraStrDup (DEFAULT_RESOLVER);
	}
	if (storageKey)
	{
		mc->storage = elektraStrDup (elektraKeyString (storageKey));
	}
	else
	{
		mc->storage = elektraStrDup (DEFAULT_STORAGE);
	}
	if (patternKey)
	{
		mc->pattern = elektraStrDup (elektraKeyString (patternKey));
	}
	else
	{
		mc->pattern = elektraStrDup (DEFAULT_PATTERN);
	}
	if (stayAliveKey) mc->stayAlive = 1;
	ElektraKey * cutKey = elektraKeyNew ("/child", ELEKTRA_KEY_END);
	ElektraKeyset * childConfig = elektraKeysetCut (config, cutKey);
	elektraKeyDel (cutKey);
	mc->childConfig = elektraKeysetRenameKeys (childConfig, "system");
	elektraKeysetAppend (config, childConfig);
	elektraKeysetDel (childConfig);
	mc->childBackends = elektraKeysetNew (0, ELEKTRA_KS_END);
	mc->modules = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraModulesInit (mc->modules, NULL);
	elektraPluginSetData (handle, mc);
	return mc;
}


static Codes initBackend (Plugin * handle, MultiConfig * mc, SingleConfig * s, ElektraKey * parentKey)
{
	unsigned long fullPathLen = strlen (mc->originalPath) + strlen (s->filename) + 2;
	char * fullPath = elektraCalloc (fullPathLen);
	snprintf (fullPath, fullPathLen, "%s/%s", mc->originalPath, s->filename);
	s->fullPath = fullPath;
	unsigned long childParentStringLen = strlen (elektraKeyName (parentKey)) + strlen (s->filename) + 2;
	char * childParentString = elektraCalloc (childParentStringLen);
	snprintf (childParentString, childParentStringLen, "%s/%s", elektraKeyName (parentKey), s->filename);
	s->parentString = childParentString;
	// fprintf (stderr, "Added file %s:(%s)\n\tChildParentKey: %s\n", s->fullPath, s->filename, s->parentString);
	Plugin * resolver = NULL;
	ElektraKeyset * resolverChildConfig = elektraKeysetDup (mc->childConfig);
	elektraKeysetAppendKey (resolverChildConfig, elektraKeyNew ("/path", ELEKTRA_KEY_VALUE, s->fullPath, ELEKTRA_KEY_END));
	resolver = elektraPluginOpen (mc->resolver, mc->modules, resolverChildConfig, parentKey);
	// fprintf (stderr, "%s:(%s)\n", keyName (parentKey), keyString (parentKey));
	if (!resolver)
	{
		// fprintf (stderr, "Failed to load resolver %s for %s\n", mc->resolver, s->parentString);
		return ERROR;
	}
	else
	{
		s->resolver = resolver;
		resolver->global = elektraPluginGetGlobalKeySet (handle);
	}
	Plugin * storage = NULL;
	ElektraKeyset * storageChildConfig = elektraKeysetDup (mc->childConfig);
	elektraKeysetAppendKey (storageChildConfig, elektraKeyNew ("system:/path", ELEKTRA_KEY_VALUE, s->fullPath, ELEKTRA_KEY_END));
	storage = elektraPluginOpen (mc->storage, mc->modules, storageChildConfig, parentKey);
	if (!storage)
	{
		// fprintf (stderr, "Failed to load storage %s for %s\n", mc->storage, s->parentString);
		return ERROR;
	}
	else
	{
		s->storage = storage;
		storage->global = elektraPluginGetGlobalKeySet (handle);
	}
	return SUCCESS;
}

static Codes resolverGet (SingleConfig * s, ElektraKeyset * returned, ElektraKey * parentKey)
{
	Plugin * resolver = s->resolver;
	int rc = resolver->kdbGet (resolver, returned, parentKey);
	s->rcResolver = rvToRc (rc);
	;
	if (s->fullPath) elektraFree (s->fullPath);
	s->fullPath = elektraStrDup (elektraKeyString (parentKey));
	return s->rcResolver;
}

static Codes updateFilesGlob (Plugin * handle, MultiConfig * mc, ElektraKeyset * found, ElektraKey * parentKey)
{
	Codes rc = NOUPDATE;
	glob_t results;
	int ret;

	char pattern[strlen (mc->directory) + strlen (mc->pattern) + 2];
	snprintf (pattern, sizeof (pattern), "%s/%s", mc->directory, mc->pattern);

	ret = glob (pattern, 0, NULL, &results);
	if (ret != 0)
	{
		if (ret == GLOB_NOSPACE)
		{
			ELEKTRA_SET_OUT_OF_MEMORY_ERROR (parentKey);
		}
		else if (ret == GLOB_ABORTED)
		{
			ELEKTRA_SET_RESOURCE_ERRORF (parentKey, "Glob(%s) failed with a read error", pattern);
		}
		else if (ret == GLOB_NOMATCH)
		{
			ELEKTRA_SET_VALIDATION_SYNTACTIC_ERRORF (parentKey, "Glob(%s) failed with no matches", pattern);
		}
		return ERROR;
	}
	struct stat sb;
	for (unsigned int i = 0; i < results.gl_pathc; ++i)
	{
		ret = stat (results.gl_pathv[i], &sb);
		if (S_ISREG (sb.st_mode))
		{
			ElektraKey * lookup = elektraKeyNew ("/", ELEKTRA_KEY_END);
			elektraKeyAddBaseName (lookup, (results.gl_pathv[i]) + strlen (mc->directory));
			ElektraKey * k;
			if ((k = elektraKeysetLookup (mc->childBackends, lookup, ELEKTRA_KDB_O_NONE)) != NULL)
			{
				elektraKeysetAppendKey (found, k);
			}
			else
			{
				SingleConfig * s = elektraCalloc (sizeof (SingleConfig));
				s->filename = elektraStrDup ((results.gl_pathv[i]) + strlen (mc->directory) + 1);
				Codes r = initBackend (handle, mc, s, parentKey);
				if (r == ERROR)
				{
					if (!mc->stayAlive)
					{
						elektraKeyDel (lookup);
						globfree (&results);
						return ERROR;
					}
					else
					{
						closeBackend (s);
					}
				}
				else
				{
					ElektraKey * childKey = elektraKeyNew (elektraKeyName (lookup), ELEKTRA_KEY_BINARY, ELEKTRA_KEY_SIZE, sizeof (SingleConfig *), ELEKTRA_KEY_VALUE,
								 &s, ELEKTRA_KEY_END);
					elektraKeysetAppendKey (mc->childBackends, childKey);
					elektraKeysetAppendKey (found, childKey);
					rc = SUCCESS;
				}
			}
			elektraKeyDel (lookup);
		}
	}
	globfree (&results);
	return rc;
}

static Codes updateFiles (Plugin * handle, MultiConfig * mc, ElektraKeyset * returned, ElektraKey * parentKey)
{
	Codes rc = NOUPDATE;
	ElektraKeyset * found = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKey * initialParent = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);

	rc = updateFilesGlob (handle, mc, found, parentKey);
	if (rc == ERROR)
	{
		elektraKeysetDel (found);
		elektraKeyDel (initialParent);
		return ERROR;
	}

	elektraKeysetRewind (mc->childBackends);
	elektraKeysetRewind (found);
	ElektraKey * c;
	ssize_t cacheHits = 0;
	ssize_t numBackends = elektraKeysetGetSize (found);
	while ((c = elektraKeysetNext (found)) != NULL)
	{
		if (elektraKeysetLookup (mc->childBackends, c, ELEKTRA_KDB_O_POP))
		{
			SingleConfig * s = *(SingleConfig **) elektraKeyValue (c);
			elektraKeySetName (parentKey, s->parentString);
			elektraKeySetString (parentKey, s->fullPath);
			int r = resolverGet (s, returned, parentKey);
			elektraFree (s->fullPath);
			s->fullPath = elektraStrDup (elektraKeyString (parentKey));
			s->rcResolver = rvToRc (r);
			if (s->rcResolver == ERROR)
			{
				if (mc->stayAlive)
				{
					elektraCursor savedCursor = elektraKeysetGetCursor (found);
					elektraKeysetAppendKey (found, c);
					elektraKeysetSetCursor (found, savedCursor);
				}
				else
				{
					rc = ERROR;
					break;
				}
			}

			if (r == ELEKTRA_PLUGIN_STATUS_CACHE_HIT)
			{
				++cacheHits;
			}

			if (r > 0)
			{
				rc = SUCCESS;
			}
		}
	}
	if (elektraKeysetGetSize (mc->childBackends) > 0 && rc != -1)
	{
		rc = SUCCESS;
		mc->hasDeleted = 1;
	}
	closeBackends (mc->childBackends);
	elektraKeysetDel (mc->childBackends);
	if (rc == ERROR)
	{
		closeBackends (found);
		elektraKeysetDel (found);
	}
	else
	{
		mc->childBackends = found;
	}
	if (cacheHits == numBackends)
	{
		rc = CACHE_HIT;
	}
	elektraKeySetName (parentKey, elektraKeyName (initialParent));
	elektraKeySetString (parentKey, elektraKeyString (initialParent));
	elektraKeyDel (initialParent);
	return rc;
}

static Codes doGetStorage (MultiConfig * mc, ElektraKey * parentKey)
{
	elektraKeysetRewind (mc->childBackends);
	ElektraKey * initialParent = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	Codes rc = NOUPDATE;
	ElektraKey * k;
	while ((k = elektraKeysetNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **) elektraKeyValue (k);
		// When we reach this stage, we will need to load
		// any successfully resolved files (as it is done in the kdb core)
		if (s->rcResolver < 0) continue;
		elektraKeySetName (parentKey, s->parentString);
		elektraKeySetString (parentKey, s->fullPath);
		Plugin * storage = s->storage;
		ElektraKeyset * readKS = elektraKeysetNew (0, ELEKTRA_KS_END);
		int r = storage->kdbGet (storage, readKS, parentKey);
		if (r > 0)
		{
			if (s->ks) elektraKeysetDel (s->ks);
			s->ks = elektraKeysetDup (readKS);
			rc = SUCCESS;
		}
		else
		{
			rc = ERROR;
		}
		elektraKeysetDel (readKS);
	}
	elektraKeySetName (parentKey, elektraKeyName (initialParent));
	elektraKeySetString (parentKey, elektraKeyString (initialParent));
	elektraKeyDel (initialParent);
	return rc;
}

static void fillReturned (MultiConfig * mc, ElektraKeyset * returned)
{
	elektraKeysetRewind (mc->childBackends);
	elektraKeysetClear (returned);
	ElektraKey * k;
	while ((k = elektraKeysetNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **) elektraKeyValue (k);
		elektraKeysetAppend (returned, s->ks);
	}
	elektraKeysetRewind (returned);
}

int elektraMultifileGet (Plugin * handle, ElektraKeyset * returned, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/multifile"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30, elektraKeyNew ("system:/elektra/modules/multifile", ELEKTRA_KEY_VALUE, "multifile plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/multifile/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/multifile/exports/open", ELEKTRA_KEY_FUNC, elektraMultifileOpen, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/multifile/exports/close", ELEKTRA_KEY_FUNC, elektraMultifileClose, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/multifile/exports/get", ELEKTRA_KEY_FUNC, elektraMultifileGet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/multifile/exports/set", ELEKTRA_KEY_FUNC, elektraMultifileSet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/multifile/exports/commit", ELEKTRA_KEY_FUNC, elektraMultifileCommit, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/multifile/exports/error", ELEKTRA_KEY_FUNC, elektraMultifileError, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/multifile/exports/checkconf", ELEKTRA_KEY_FUNC, elektraMultifileCheckConf, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/multifile/exports/checkfile", ELEKTRA_KEY_FUNC, elektraMultifileCheckFile, ELEKTRA_KEY_END),

#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/multifile/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; // success
	}
	// get all keys
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc)
	{
		mc = initialize (handle, parentKey);
	}
	if (!mc)
	{
		return -1;
	}
	Codes rc = NOUPDATE;
	if (mc->getPhase == MULTI_GETRESOLVER)
	{
		rc = updateFiles (handle, mc, returned, parentKey);
		// if it is a only a partial cache hit, we still need to load everything
		// in the next phase
		if (rc >= SUCCESS)
		{
			mc->getPhase = MULTI_GETSTORAGE;
		}
	}
	else if (mc->getPhase == MULTI_GETSTORAGE)
	{
		rc = doGetStorage (mc, parentKey);
		if (rc == SUCCESS || mc->hasDeleted)
		{
			fillReturned (mc, returned);
			mc->hasDeleted = 0;
		}
		mc->getPhase = MULTI_GETRESOLVER;
	}
	elektraPluginSetData (handle, mc);
	if (rc == CACHE_HIT)
	{
		return ELEKTRA_PLUGIN_STATUS_CACHE_HIT;
	}
	else if (rc == SUCCESS)
	{
		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	else if (rc == NOUPDATE)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}
	else
	{
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
}

static Codes resolverSet (MultiConfig * mc, ElektraKey * parentKey)
{
	elektraKeysetRewind (mc->childBackends);
	ElektraKey * initialParent = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	ElektraKey * k;
	Codes rc = NOUPDATE;
	while ((k = elektraKeysetNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **) elektraKeyValue (k);
		if (s->rcResolver == NOUPDATE)
		{
			// fprintf (stderr, "SKIPPING %s:(%s)\n", s->parentString, s->fullPath);
			continue;
		}
		else if (s->rcResolver == EMPTY)
		{
			// fprintf (stderr, "MARK FOR DELETE: %s:(%s)\n", s->parentString, s->fullPath);
			// ++rc;
			continue;
		}
		else
		{
			// fprintf (stderr, "UPDATE: %s:(%s)\n", s->parentString, s->fullPath);
		}

		Plugin * resolver = s->resolver;
		elektraKeySetName (parentKey, s->parentString);
		elektraKeySetString (parentKey, s->fullPath);
		int r = resolver->kdbSet (resolver, s->ks, parentKey);
		s->rcResolver = rc = rvToRc (r);
		if (rc == ERROR) break;
		if (s->tmpFilename) elektraFree (s->tmpFilename);
		s->tmpFilename = elektraStrDup (elektraKeyString (parentKey));
		// fprintf (stderr, "tmp filename for %s: %s\n", s->fullPath, s->tmpFilename);
	}
	elektraKeySetName (parentKey, elektraKeyName (initialParent));
	elektraKeySetString (parentKey, elektraKeyString (initialParent));
	elektraKeyDel (initialParent);
	return rc;
}

static Codes doSetStorage (MultiConfig * mc, ElektraKey * parentKey)
{
	elektraKeysetRewind (mc->childBackends);
	ElektraKey * initialParent = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	Codes rc = NOUPDATE;
	ElektraKey * k;
	while ((k = elektraKeysetNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **) elektraKeyValue (k);
		if (s->rcResolver == EMPTY)
		{
			rc = SUCCESS;
			s->rcStorage = EMPTY;
			continue;
		}
		else if (s->rcResolver != SUCCESS)
		{
			continue;
		}

		elektraKeySetName (parentKey, s->parentString);
		elektraKeySetString (parentKey, s->tmpFilename);
		Plugin * storage = s->storage;
		int r = storage->kdbSet (storage, s->ks, parentKey);
		s->rcStorage = rc = rvToRc (r);

		// fprintf (stderr, "%s ->kdbSet returned %d\n", mc->storage, r);

		if (rc == ERROR) break;
	}
	elektraKeySetName (parentKey, elektraKeyName (initialParent));
	elektraKeySetString (parentKey, elektraKeyString (initialParent));
	elektraKeyDel (initialParent);
	return rc;
}

static Codes doCommit (MultiConfig * mc, ElektraKey * parentKey)
{
	elektraKeysetRewind (mc->childBackends);
	ElektraKey * initialParent = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	Codes rc = NOUPDATE;
	ElektraKey * k;
	while ((k = elektraKeysetNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **) elektraKeyValue (k);
		if (s->rcStorage == EMPTY)
		{
			unlink (s->fullPath);
			continue;
		}
		else if (s->rcStorage != 1)
		{
			continue;
		}

		elektraKeySetName (parentKey, s->parentString);
		elektraKeySetString (parentKey, s->fullPath);
		Plugin * resolver = s->resolver;
		int r = resolver->kdbSet (resolver, s->ks, parentKey);
		rc = rvToRc (r);
		if (rc == ERROR) break;
		// fprintf (stderr, "%s ->kdbSet returned %d\n", mc->resolver, r);
	}
	elektraKeySetName (parentKey, elektraKeyName (initialParent));
	elektraKeySetString (parentKey, elektraKeyString (initialParent));
	elektraKeyDel (initialParent);
	return rc;
}

static int diffOrNeedSync (ElektraKeyset * ks, ElektraKeyset * checkKS)
{
	if (elektraKeysetGetSize (ks) != elektraKeysetGetSize (checkKS)) return 1;
	elektraKeysetRewind (ks);
	elektraKeysetRewind (checkKS);
	ElektraKey * key = NULL;
	ElektraKey * check = NULL;
	int ret = -1;
	while (ret == -1)
	{
		key = elektraKeysetNext (ks);
		check = elektraKeysetNext (checkKS);
		if (!key && !check)
		{
			ret = 0;
		}
		else if (!key || !check)
		{
			ret = 1;
		}
		else if (elektraKeyCmp (key, check))
		{
			ret = 1;
		}
		else if (elektraKeyNeedSync (check))
		{
			ret = 1;
		}
	}
	return ret;
}

static void flagUpdateBackends (MultiConfig * mc, ElektraKeyset * returned)
{
	elektraKeysetRewind (mc->childBackends);
	ElektraKey * k;
	while ((k = elektraKeysetNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **) elektraKeyValue (k);
		ElektraKey * cutKey = elektraKeyNew (s->parentString, ELEKTRA_KEY_END);
		ElektraKeyset * cutKS = elektraKeysetCut (returned, cutKey);
		if (elektraKeysetGetSize (cutKS) == 0)
		{
			s->rcResolver = EMPTY;
			if (s->ks) elektraKeysetDel (s->ks);
			s->ks = NULL;
		}
		else if (diffOrNeedSync (s->ks, cutKS))
		{
			s->rcResolver = SUCCESS;
			if (s->ks) elektraKeysetDel (s->ks);
			s->ks = elektraKeysetDup (cutKS);
		}
		else
		{
			s->rcResolver = NOUPDATE;
		}
		elektraKeyDel (cutKey);
		elektraKeysetDel (cutKS);
	}
}

int elektraMultifileSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc) return -1;
	Codes rc = NOUPDATE;
	if (mc->setPhase == MULTI_SETRESOLVER)
	{
		flagUpdateBackends (mc, returned);
		rc = resolverSet (mc, parentKey);
		// fprintf (stderr, "resolverSet returned %d\n", rc);
		if (rc == ERROR)
		{
			return -1;
		}
		mc->setPhase = MULTI_SETSTORAGE;
	}
	else if (mc->setPhase == MULTI_SETSTORAGE)
	{
		rc = doSetStorage (mc, parentKey);
		if (rc == ERROR)
		{
			return -1;
		}
		mc->setPhase = MULTI_COMMIT;
	}
	else if (mc->setPhase == MULTI_COMMIT)
	{
		doCommit (mc, parentKey);
		mc->setPhase = MULTI_SETRESOLVER;
	}
	if (rc == SUCCESS)
	{
		return 1;
	}
	else if (rc == NOUPDATE)
	{
		return 0;
	}
	else
	{
		return -1;
	}
}

int elektraMultifileError (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	MultiConfig * mc = elektraPluginGetData (handle);
	if (!mc) return 0;
	elektraKeysetRewind (mc->childBackends);
	ElektraKey * key;
	ElektraKey * initialParent = elektraKeyDup (parentKey, ELEKTRA_KEY_CP_ALL);
	while ((key = elektraKeysetNext (mc->childBackends)) != NULL)
	{
		SingleConfig * s = *(SingleConfig **) elektraKeyValue (key);
		Plugin * resolver = s->resolver;
		elektraKeySetName (parentKey, s->parentString);
		elektraKeySetString (parentKey, s->fullPath);
		if (resolver->kdbError)
		{
			resolver->kdbError (resolver, returned, parentKey);
		}
	}
	elektraKeySetName (parentKey, elektraKeyName (initialParent));
	elektraKeySetString (parentKey, elektraKeyString (initialParent));
	elektraKeyDel (initialParent);


	return 1; // success
}

int elektraMultifileCommit (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	return elektraMultifileSet (handle, returned, parentKey);
}

int elektraMultifileCheckConf (ElektraKey * errorKey ELEKTRA_UNUSED, ElektraKeyset * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	// the return codes have the following meaning:
	// 0: The configuration was OK and has not been changed
	// 1: The configuration has been changed and now it is OK
	// -1: The configuration was not OK and could not be fixed. An error has to be set to errorKey.
	return 0;
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
    return elektraPluginExport ("multifile",
	    ELEKTRA_PLUGIN_OPEN,	&elektraMultifileOpen,
	    ELEKTRA_PLUGIN_CLOSE,	&elektraMultifileClose,
	    ELEKTRA_PLUGIN_GET,	&elektraMultifileGet,
	    ELEKTRA_PLUGIN_SET,	&elektraMultifileSet,
	    ELEKTRA_PLUGIN_ERROR,	&elektraMultifileError,
	    ELEKTRA_PLUGIN_COMMIT,      &elektraMultifileCommit,
	    ELEKTRA_PLUGIN_END);
}


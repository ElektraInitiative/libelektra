#include <kdbmount.h>
#include <kdbhelper.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <kdbmerge.h>

#include <kdbmodule.h>
#include <kdbprivate.h> // currently needed for plugin handling (struct _Plugin)
#include <kdbconfig.h> // for HAVE_GLOB

#ifdef HAVE_GLOB
#include <glob.h>
#endif

#include <ctype.h>
#include <kdblogger.h>


struct PluginSpec{
	char * name;
	char * refname;
	KeySet * config;
};

enum PluginStatus
{
	/* works not directly, but can be loaded via provides */
	provides,
	/* exists and working as given */
	real,
	/* does not exist or cannot be loaded */
	missing
};


/* Read mount configuration from the KDB
 * Make sure to ksDel(...) the returned KeySet */
KeySet * getMountConfig (KDB * handle, Key * errorKey, const char * const mountpointsPath)
{
	Key * parent = NULL;

	if (!mountpointsPath || !*mountpointsPath)
		parent = keyNew (DEFAULT_MOUNTPOINTS_PATH, KEY_END);
	else
		parent = keyNew (mountpointsPath, KEY_END);

	KeySet * mountInfo = ksNew (0, KS_END);
	if (kdbGet (handle, mountInfo, parent) == -1)
	{
		/* TODO: Implement error handling */
	}

	/* TODO: maybe print warnings(or add them to error key) */

	keyDel (parent);
	return mountInfo;
}


void cOutputMtab (KeySet * mountConf, bool clFirst, bool clSecond, bool clNull)
{
	// in c++: Vector with BackendInfo-structs
	KeySet * mtab = getBackendInfo (mountConf);
	char delim = clNull ? '\0' : '\n';

	for (elektraCursor it = 0; it < ksGetSize (mtab); ++it)
	{
		Key * cur = ksAtCursor (mtab, it);
		if (!clFirst)
		{
			printf ("%s", keyString (cur));
			if (!clSecond)
			{
				printf (" on ");
			}
			else
			{
				printf("%s%c", keyName (cur), delim);
			}
		}

		if (!clSecond)
		{
			printf ("%s%c", keyName (cur), delim);
		}
	}

	ksDel (mtab);
}


bool isValidMountPoint (Key * mountPoint, KeySet * mountConf)
{
	KeySet * backendInfos = getBackendInfo (mountConf);
	KeySet * alreadyUsedMountpoints = ksNew (0, KS_END);

	/* handle cascading mountpoints (multiple namespaces) */
	for (elektraCursor it = 0; it < ksGetSize (backendInfos); ++it)
	{
		Key * cur = ksAtCursor (backendInfos, it);
		if (cur)
		{
			const char * curKeyName = keyName (cur);
			if (keyGetNameSize (cur) == 2 && *curKeyName == '/')
			{
				Key * curNamespaceKey = keyNew ("spec:/", KEY_END);
				ksAppendKey (alreadyUsedMountpoints, curNamespaceKey);

				curNamespaceKey = keyNew ("dir:/", KEY_END);
				ksAppendKey (alreadyUsedMountpoints, curNamespaceKey);

				curNamespaceKey = keyNew ("user:/", KEY_END);
				ksAppendKey (alreadyUsedMountpoints, curNamespaceKey);

				curNamespaceKey = keyNew ("system:/", KEY_END);
				ksAppendKey (alreadyUsedMountpoints, curNamespaceKey);
			}
			else if (keyGetNameSize (cur) > 1 && *curKeyName == '/')
			{
				char * tmpStr = elektraMalloc (strlen("system:") + keyGetNameSize(cur)); /* includes \0 */

				strcpy(tmpStr, "dir:");
				strcat (tmpStr, curKeyName);
				Key * curNamespaceKey = keyNew (tmpStr, KEY_END);
				ksAppendKey (alreadyUsedMountpoints, curNamespaceKey);

				strcpy(tmpStr, "user:");
				strcat (tmpStr, curKeyName);
				curNamespaceKey = keyNew (tmpStr, KEY_END);
				ksAppendKey (alreadyUsedMountpoints, curNamespaceKey);

				strcpy(tmpStr, "system:");
				strcat (tmpStr, curKeyName);
				curNamespaceKey = keyNew (tmpStr, KEY_END);
				ksAppendKey (alreadyUsedMountpoints, curNamespaceKey);

				elektraFree (tmpStr);
			}

			/* always add current key itself, too */
			ksAppendKey (alreadyUsedMountpoints, cur);
		}
		else
		{
			/* TODO: handle error */
		}
	}

	/* STEP 0: CHeck for null key */
	if (!mountPoint)
	{
		// TODO: set error key (was exception in c++ code)
		fprintf (stderr, "Null mountpoint not allowed!\n");
		ksDel (backendInfos);
		ksDel (alreadyUsedMountpoints);
		return false;
	}

	/* STEP 1: Check for empty name */
	if (keyGetNameSize (mountPoint) < 2) /* \0 is counted --> len >= 2 for non-empty string */
	{
		// TODO: set error key (was exception in c++ code)
		fprintf (stderr, "Empty mountpoint not allowed!\n");
		ksDel (backendInfos);
		ksDel (alreadyUsedMountpoints);
		return false;
	}

	/* STEP 2: Check for wrong namespace (proc) */
	if (keyGetNamespace (mountPoint) == KEY_NS_PROC)
	{
		// TODO: set error key (was exception in c++ code)
		fprintf (stderr, "proc:/ mountpoint not allowed!\n");
		ksDel (backendInfos);
		ksDel (alreadyUsedMountpoints);
		return false;
	}

	/* STEP 3: Check for name match */
	const char * mpName = keyName (mountPoint);
	if (keyGetNameSize (mountPoint) == 2 && *mpName == '/')
	{

		if (ksLookupByName (alreadyUsedMountpoints, "/", KDB_O_NONE))
		{
			// TODO: set error key (was exception in c++ code)
			fprintf (stderr, "Root mountpoint not possible, because the root mountpoint already exists!\n");
			ksDel (backendInfos);
			ksDel (alreadyUsedMountpoints);
			return false;
		}

		if (ksLookupByName (alreadyUsedMountpoints, "spec:/", KDB_O_NONE))
		{
			// TODO: set error key (was exception in c++ code)
			fprintf (stderr, "Root mountpoint not possible, because spec mountpoint already exists!\n");
			ksDel (backendInfos);
			ksDel (alreadyUsedMountpoints);
			return false;
		}

		if (ksLookupByName (alreadyUsedMountpoints, "dir:/", KDB_O_NONE))
		{
			// TODO: set error key (was exception in c++ code)
			fprintf (stderr, "Root mountpoint not possible, because dir mountpoint already exists!\n");
			ksDel (backendInfos);
			ksDel (alreadyUsedMountpoints);
			return false;
		}

		if (ksLookupByName (alreadyUsedMountpoints, "user:/", KDB_O_NONE))
		{
			// TODO: set error key (was exception in c++ code)
			fprintf (stderr, "Root mountpoint not possible, because user mountpoint already exists!\n");
			ksDel (backendInfos);
			ksDel (alreadyUsedMountpoints);
			return false;
		}

		if (ksLookupByName (alreadyUsedMountpoints, "system:/", KDB_O_NONE))
		{
			// TODO: set error key (was exception in c++ code)
			fprintf (stderr, "Root mountpoint not possible, because system mountpoint already exists!\n");
			ksDel (backendInfos);
			ksDel (alreadyUsedMountpoints);
			return false;
		}

	}
	else if (keyGetNameSize (mountPoint) > 2 && *mpName == '/')
	{
		if (ksLookupByName (alreadyUsedMountpoints, mpName, KDB_O_NONE))
		{
			// TODO: set error key (was exception in c++ code)
			fprintf (stderr, "Cascading mountpoint %s not possible, because cascading mountpoint %s already exists.\n", mpName, mpName);ksDel (backendInfos);
			ksDel (alreadyUsedMountpoints);
			return false;
		}

		char * tmpStr = elektraMalloc (keyGetNameSize (mountPoint) + strlen ("system:"));

		strcpy (tmpStr, "dir:");
		strcat (tmpStr, mpName);
		if (ksLookupByName (alreadyUsedMountpoints, tmpStr, KDB_O_NONE))
		{
			// TODO: set error key (was exception in c++ code)
			fprintf (stderr, "Cascading mountpoint %s not possible, because dir mountpoint already exists.\n", mpName);
			ksDel (backendInfos);
			ksDel (alreadyUsedMountpoints);
			return false;
		}

		strcpy (tmpStr, "user:");
		strcat (tmpStr, mpName);
		if (ksLookupByName (alreadyUsedMountpoints, tmpStr, KDB_O_NONE))
		{
			// TODO: set error key (was exception in c++ code)
			fprintf (stderr, "Cascading mountpoint %s not possible, because user mountpoint already exists.\n", mpName);
			ksDel (backendInfos);
			ksDel (alreadyUsedMountpoints);
			return false;
		}

		strcpy (tmpStr, "system:");
		strcat (tmpStr, mpName);
		if (ksLookupByName (alreadyUsedMountpoints, tmpStr, KDB_O_NONE))
		{
			// TODO: set error key (was exception in c++ code)
			fprintf (stderr, "Cascading mountpoint %s not possible, because system mountpoint already exists.\n", mpName);
			ksDel (backendInfos);
			ksDel (alreadyUsedMountpoints);
			return false;
		}

		elektraFree (tmpStr);
	}
	else
	{
		if (ksLookupByName (alreadyUsedMountpoints, mpName, KDB_O_NONE))
		{
			// TODO: set error key (was exception in c++ code)
			// TODO: list used names in error message (see /src/libs/tools/src/backend.cpp:213)
			fprintf (stderr, "Mountpoint %s is one of the already used names!\n", mpName);
			ksDel (backendInfos);
			ksDel (alreadyUsedMountpoints);
			return false;
		}
	}

	ksDel (alreadyUsedMountpoints);
	ksDel (backendInfos);

	/* TODO: STEP 4: Check if mounted below system:/elektra */
	Key * elektraCheck = keyDup (mountPoint, KEY_CP_NAME);

	/* Remove namespace */
	for (int i = 0; i < keyGetNameSize (elektraCheck); ++i)
	{
		if (*(mpName + i) == ':')
		{
			keySetName (elektraCheck, mpName + i + 1);
		}
	}

	Key * elektraKey = keyNew ("/elektra", KEY_END);
	if (keyIsBelowOrSame (elektraKey, elektraCheck))
	{
		// TODO: set error key (was exception in c++ code)
		fprintf (stderr, "Mountpoint %s is below the reserved names /elektra because it would cause inconsistencies in this or future versions.\n", mpName);
	}

	keyDel (elektraKey);
	keyDel (elektraCheck);


	/* Everything worked */
	return true;
}

/**
 * @brief give info about current mounted backends
 *
 * @param mountConf a keyset that contains everything below
 * DEFAULT_MOUNTPOINTS_PATH
 *
 * @return A KeySet with Keys that contain information about mounted backends.
 * The keyname is the mountpoint, the value of the key is the path.
 * The returned KeySet has to be freed (ksDel()) by the caller.
 */
KeySet * getBackendInfo (KeySet * mountConf)
{
	if (!mountConf || ksGetSize (mountConf) <= 0)
	{
		return NULL;
	}

	Key * rootKey = keyNew (DEFAULT_MOUNTPOINTS_PATH, KEY_END);
	KeySet * result = ksNew (0, KS_END);

	if (!rootKey || !result)
	{
		/* TODO: error handling */
	}

	for (elektraCursor it = 0; it < ksGetSize (mountConf); ++it)
	{
		Key * cur = ksAtCursor (mountConf, it);
		if (keyIsDirectlyBelow(rootKey, cur))
		{
			size_t lenLookup = keyGetNameSize (cur); /* includes \0 */
			if (lenLookup > 1)
			{
				lenLookup += strlen ("/definition/path");
				char * strLookup = elektraMalloc (lenLookup);
				strcpy (strLookup, keyName (cur));
				strcat (strLookup, "/definition/path");
				Key * path = ksLookupByName (mountConf, strLookup, KDB_O_NONE);
				elektraFree (strLookup);

				/* keyname = mountpoint */
				Key * curBackendInfo = keyNew (keyBaseName (cur), KEY_END);
				if (path)
				{
					/* value of key = path */
					keySetString (curBackendInfo, keyString (path));
				}
				ksAppendKey (result, curBackendInfo);
			}
			else
			{
				/* TODO: handle error */
			}
		}
	}

	keyDel (rootKey);
	return result;
}





/* Plugin related stuff */

/**
 * @brief Check if str starts with a-z and then only has chars a-z, 0-9 or underscore (_)
 *
 * @param str the string to check
 *
 * @return true if the provided str is a valid plugin-name, false otherwise
 */
bool validatePluginName (const char * str)
{
	/* must not be null or empty */
	if (!str || !*str)
	{
		return false;
	}

	/* must start with a-z */
	if (*str < 'a' || *str > 'z')
	{
		return false;
	}

	while (*++str)
	{
		if ((*str < 'a' || *str > 'z') && (*str < '0' || *str > '9') && *str != '_')
		{
			return false;
		}
	}

	return true;
}

/**
 * @brief Set the full name (name + refname) with # or only the name
 *
 * @param ps the PluginSpec for which the name(s) should be set
 * @param str the name to set, must be valid as long as `ps` is in use (caller is responsible for freeing)
 *
 * @return true if the str was valid, false otherwise
 */
bool setPluginFullName (struct PluginSpec * ps, char * const str)
{
	if (validatePluginName (str))
	{
		char *it = str;
		while (*it++)
		{
			if (*it == '#')
			{
				/* split the string */
				*it = '\0';
				it++;
				ps->name = str;
				ps->refname = it;
				return true;
			}
		}

		/* no '#' found --> treat as name without refname */
		ps->name = str;
		return true;
	}
	else
	{
		return false;
	}
}

/** Get the full name of the plugin from the PluginSpec
 * @param ps The PluginSpec to get the name from
 * @returns The full name of the plugin, make sure to free the returned string!
 */
char * getPluginFullName (struct PluginSpec ps)
{
	char * result = NULL;

	if (ps.refname && *(ps.refname))
	{
		/* count \0 for both strings, use one byte for the "#" char */
		result = elektraMalloc (elektraStrLen (ps.name) + elektraStrLen (ps.refname));
		strcpy (result, ps.name);
		strcat (result, "#");
		strcat (result, ps.refname);

	}
	else
	{
		result = elektraMalloc (elektraStrLen (ps.name));
		strcpy (result, ps.name);
	}

	return result;
}



/** @brief Parse a string containing information to create a KeySet
 *  @param pluginArguments comma (,) to separate key=value, contains no whitespaces
 *  @return newly created keyset with the information found in the string, make sure to ksDel() the returned KeySet.
 *  @return NULL if no '=' was found in pluginArguments
 */
const KeySet * cParsePluginArguments (char * pluginArguments, const char * const basepath)
{
	KeySet * ks = NULL;

	/* Read until the next '=', this should be the key name */
	for (size_t posEqual, posComma; (posEqual = strcspn (pluginArguments, "=")) > 0; pluginArguments = pluginArguments + posComma + 1)
	{
		/* Replace '=' with '\0' to use the current substring as parameter for a keyname */
		pluginArguments[posEqual] = 0;
		/* TODO: Handle error ('=' not found) */

		posComma = strcspn (pluginArguments + posEqual + 1, ",");
		pluginArguments[posComma] = 0;

		/* elektraStrLen includes '\0', so we counted it two times, but use one byte for the '/' character */
		char * pluginKeyName = elektraMalloc (elektraStrLen (pluginArguments) + elektraStrLen (basepath));
		strcpy (pluginKeyName, basepath);
		strcat (pluginKeyName, "/");
		strcat (pluginKeyName, pluginArguments);

		Key * const keyToAppend = keyNew (pluginKeyName, KEY_VALUE, pluginArguments + posEqual + 1, KEY_END);

		/* The string gets copied by keyNew(), so we have to free the memory here. */
		elektraFree (pluginKeyName);

		if (!ks)
		{
			ks = ksNew (0, KS_END);
		}
		ksAppendKey (ks, keyToAppend);
	}

	return ks;
}


int pstrcmp (const void *a, const void *b)
{
	char * const * pa = a;
	char * const * pb = b;
	return strcmp(*pa, *pb);
}


/* from plugindatabase.cpp[52-98] */
/* For the returned string-array, make sure to free the individual strings AND the array that holds the char*-pointers */
char** getAllPluginNames (void)
{
	char **ret = NULL; /* Last element must be NULL */
	size_t retIndex = 0;

#ifndef ELEKTRA_SHARED
#ifdef HAVE_GLOB
	const char *toIgnore[] = {"proposal", "core", "ease", "meta", "plugin", "full", "kdb", "static", NULL};
	glob_t pglob;

	if (glob (BUILTIN_PLUGIN_FOLDER "/libelektra-*", GLOB_NOSORT, NULL, &pglob) == 0)
	{
		ELEKTRA_LOG ("has glob %zd", pglob.gl_pathc);

		/* iterate over matched pathnames */
		ret = elektraMalloc ((pglob.gl_pathc + 1) * sizeof(char*));
		for (size_t i = 0; i < pglob.gl_pathc; ++i)
		{
			char * curPathname = pglob.gl_pathv[i];
			/* find last '-' in string */
			size_t startPos = -1;
			for (size_t j = 0; curPathname[j] != '\0'; ++j)
			{
				if (curPathname[j] == '-')
				{
					startPos = j;
				}
			}

			if (startPos == (size_t)(-1))
			{
				// ignore wrong file
				continue;
			}

			/* find first '.' after the last '-' */
			size_t endPos;
			for (endPos = startPos; curPathname[endPos + 1] != '\0' && curPathname[endPos + 1] != '.'; ++endPos);

			if (endPos > startPos && curPathname[endPos + 1] == '.')
			{
				++endPos;
			}
			else
			{
				// ignore wrong file
				continue;
			}

			/* Copy substring (start after last '-' and end before the next '.') */
			/* TODO: Assert that (endPos - startPos) > 1 */
			char * extractedName = elektraMalloc ((endPos - startPos) * sizeof(char));
			for (size_t j = startPos + 1; j < endPos; ++j)
				extractedName[j - startPos - 1] = curPathname[j];
			extractedName[endPos - startPos - 1] = '\0';

			/* check if the entry should be ignored */
			bool ignoreEntry = false;
			for (i = 0; *(toIgnore + i); i++)
			{
				if (strcmp (extractedName, *(toIgnore + i)) == 0)
				{
					ignoreEntry = true;
					break;
				}
			}

			if (!ignoreEntry)
			{
				ret[retIndex++] = extractedName;
			}
		}
		ret[retIndex] = NULL; /* last element (like '\0' in strings) */
		globfree (&pglob);
		/* TODO: Resize ret-array if optimizing for small size */
	}
#undef IGNORE_COUNT
#endif /* HAVE_GLOB */

	if (retIndex > 0)
	{
		/* C++: std::sort (ret.begin (), ret.end ()); */
		qsort (*ret, retIndex, sizeof (char *), pstrcmp);
		return ret;
	}
	else
	{
		elektraFree (ret);
	}



/* if we did not find plugins, return builtinPlugins
   (even if they might be wrong for ELEKTRA_SHARED) */
#endif /* ELEKTRA_SHARED */

	/* count number of plugins */
	size_t numPlugins = 1;
	for (size_t i = 0; i < elektraStrLen (ELEKTRA_PLUGINS); ++i)
	{
		if (ELEKTRA_PLUGINS[i] == ';')
			++numPlugins;
	}

	ret = elektraMalloc ((numPlugins + 1) * sizeof (char *));

	char * builtinPlugins = elektraStrDup (ELEKTRA_PLUGINS);

	if (builtinPlugins && *builtinPlugins)
	{
		retIndex = 1;
		ret[0] = builtinPlugins;
		for (size_t i = 0; builtinPlugins[i] != '\0'; ++i)
		{
			if (builtinPlugins[i] == ';')
			{
				builtinPlugins[i] = '\0';
				/* TODO: assert (retIndex < numPlugins) */
				ret[retIndex++] = builtinPlugins + i + 1;
			}
		}
		ret[retIndex] = NULL; /* last element */
	}
	else
	{
		/* TODO: handle error */
		elektraFree (ret);
		return NULL;
	}


	qsort (ret, retIndex, sizeof (char *), pstrcmp);

	/* Remove duplicates */
	for (size_t i = 1; i < retIndex; ++i)
	{
		if (strcmp (ret[i-1], ret[i]) == 0)
		{
			/* remove duplicate and move subsequent items one step up */
			--retIndex;
			for (size_t j = i; j < retIndex; ++j)
			{
				/* No freeing here, because the memory for all strings in ret must be freed at once
				 * by calling elektraFree (*ret) and then elektraFree (ret) to free the array with char pointers */
				ret[j] = ret[j + 1];
			}
			ret[retIndex] = NULL; /* remove last element */
		}
	}


	/* TODO: resize ret when optimizing for small size */
	return ret;
}

/* The provided array must have NULL as the last element (no NULL before an element, no element after a NULL) */
void freeStrArr (char ** strArr)
{
/* A continuous memory section was used for all strings in the string array, so we just have to free *strArr and strArr */
	elektraFree (*strArr);
	elektraFree (strArr);
}



long calculateStatus (char * strStatus)
{
	/* TODO: Directly use data from CONTRACT.INI */
	// clang-format off
	KeySet * ksStatusMap = ksNew (31,
		keyNew ("default", KEY_VALUE, 64000, KEY_END),
		keyNew ("recommended", KEY_VALUE, 32000, KEY_END),
		keyNew ("productive", KEY_VALUE, 8000, KEY_END),
		keyNew ("maintained", KEY_VALUE, 4000, KEY_END),
		keyNew ("reviewed", KEY_VALUE, 4000, KEY_END),
		keyNew ("conformant", KEY_VALUE, 2000, KEY_END),
		keyNew ("compatible", KEY_VALUE, 2000, KEY_END),
		keyNew ("coverage", KEY_VALUE, 2000, KEY_END),
		keyNew ("specific", KEY_VALUE, 1000, KEY_END),

		keyNew ("unittest", KEY_VALUE, 1000, KEY_END),
		keyNew ("shelltest", KEY_VALUE, 1000, KEY_END),
		keyNew ("tested", KEY_VALUE, 500, KEY_END),
		keyNew ("nodep", KEY_VALUE, 250, KEY_END),
		keyNew ("libc", KEY_VALUE, 250, KEY_END),
		keyNew ("configurable", KEY_VALUE, 50, KEY_END),
		keyNew ("final", KEY_VALUE, 50, KEY_END),
		keyNew ("global", KEY_VALUE, 1, KEY_END),
		keyNew ("readonly", KEY_VALUE, 0, KEY_END),
		keyNew ("writeonly", KEY_VALUE, 0, KEY_END),
		keyNew ("preview", KEY_VALUE, -50, KEY_END),
		keyNew ("memleak", KEY_VALUE, -250, KEY_END),
		keyNew ("experimental", KEY_VALUE, -500, KEY_END),
		keyNew ("difficult", KEY_VALUE, -500, KEY_END),
		keyNew ("limited", KEY_VALUE, -750, KEY_END),
		keyNew ("unfinished", KEY_VALUE, -1000, KEY_END),
		keyNew ("old", KEY_VALUE, -1000, KEY_END),
		keyNew ("nodoc", KEY_VALUE, -1000, KEY_END),
		keyNew ("concept", KEY_VALUE, -2000, KEY_END),
		keyNew ("orphan", KEY_VALUE, -4000, KEY_END),
		keyNew ("obsolete", KEY_VALUE, -4000, KEY_END),
		keyNew ("discouraged", KEY_VALUE, -32000, KEY_END),
		KS_END
	);
	// clang-format on

	long ret = 0;
	for (char *it = strStatus, *strPrevStart = strStatus; it && *it; ++it)
	{
		if (isspace (*it))
		{
			*it = '\0';
			Key * keyStatus = ksLookupByName (ksStatusMap, strPrevStart, KDB_O_NONE);

			if (keyStatus)
			{
				ret += *(long*) keyValue (keyStatus);
			}
			else
			{
				char *eptr;
				long numStatus = strtol (strPrevStart, &eptr, 10);

				if (!(*eptr))
				{
					ret += numStatus;
				}
			}
			strPrevStart = it + 1;
		}
	}

	ksDel (ksStatusMap);
	return ret;
}


/* returned string is part of a key in the KeySet `info` */
const char * pluginLookupInfo (struct PluginSpec ps, KeySet * info, char * item, char * section)
{
	Key * k = keyNew ("system:/elektra/modules", KEY_END);
	keyAddBaseName (k, ps.name);
	keyAddBaseName (k, section);
	keyAddBaseName (k, item);
	Key * ret = ksLookup (info, k, KDB_O_NONE);
	keyDel (k);

	if (!ret)
	{
		/* TODO: Let's say missing info is ok for now */
		return "";
	}
	else
	{
		//char * strRet = elektraMalloc (keyGetValueSize (ret));
		//keyGetString (ret, strRet, keyGetValueSize (ret));
		return keyString (ret);
	}
}

const char * modulesPluginDatabaseLookupInfo (struct PluginSpec ps, KeySet * ksInfo, char * info)
{
	ksAppendKey (ps.config, keyNew ("system:/module", KEY_VALUE, "this plugin was loaded for the status", KEY_END));
	return pluginLookupInfo (ps, ksInfo, info, "infos");
}


KeySet * lookupAllProvidesWithStatus (const char * const pluginName, KeySet * ksInfo)
{
	char **allPluginNames = getAllPluginNames();
	KeySet * ksFoundPlugins = ksNew (0, KS_END);

	struct PluginSpec ps;
	ps.config = ksNew (5, keyNew("system:/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END);

	for (char **curPluginName = allPluginNames; curPluginName; ++curPluginName)
	{
		/* TODO: make sure (non)-equal plugins (i.e. with same/different contract) are handled correctly */

		if (!validatePluginName (*curPluginName))
		{
			/* TODO: handle error */
			ksDel (ps.config);
			freeStrArr (allPluginNames);
			return NULL;
		}

		setPluginFullName (&ps, *curPluginName);


		/* Let's see if there is a plugin named after the required provider */
		if (elektraStrCmp (*curPluginName, pluginName) == 0)
		{
			//C++: int s = calculateStatus (lookupInfo (spec, "status"));
			const char * luInfo = modulesPluginDatabaseLookupInfo (ps, ksInfo, "status");

			if (luInfo)
			{
				char * dupInfo = elektraStrDup (luInfo);
				long s = calculateStatus (dupInfo);
				ksAppendKey (ksFoundPlugins, keyNew(*curPluginName, KEY_VALUE, s, KEY_END));
				elektraFree (dupInfo);

				/* We are done with the current plugin */
				continue;
			}
		}

		/* TODO: Support for generic plugins with config */
		const char * luInfo = modulesPluginDatabaseLookupInfo (ps, ksInfo, "provides");
		char * dupInfo = elektraStrDup (luInfo);

		for (char *it = dupInfo, *strPrevStart = dupInfo; it && *it; ++it)
		{
			if (isspace (*it))
			{
				*it = '\0';

				if (elektraStrCmp (strPrevStart, pluginName) == 0)
				{
					const char * luInfoProvides = modulesPluginDatabaseLookupInfo (ps, ksInfo, "status");
					char * dupInfoProvides = elektraStrDup (luInfoProvides);
					long s = calculateStatus (dupInfoProvides);
					elektraFree (dupInfoProvides);
					ksAppendKey (ksFoundPlugins, keyNew (*curPluginName, KEY_VALUE, s, KEY_END));
				}

				strPrevStart = it + 1;
			}
		}
		elektraFree (dupInfo);
	}

	if (ksGetSize (ksFoundPlugins) == 0)
	{
		/* TODO: Error handling */
	}

	return ksFoundPlugins;
}


/* from plugindatabase.cpp[104-139] */
bool hasProvides (KeySet * ksInfo, const char * const infoProvides)
{
	char **allPlugins = getAllPluginNames();

	for (char ** curPlugin = allPlugins; curPlugin; ++curPlugin)
	{
		//C++: pd.lookupInfo (...)
		struct PluginSpec ps;
		setPluginFullName (&ps, *curPlugin);
		KeySet * ksPluginConfig = ksNew (5, keyNew ("system:/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END);
		ps.config = ksPluginConfig;

		const char * const strLookupInfo = modulesPluginDatabaseLookupInfo (ps, ksInfo, "provides");
		char * dupStrLookupInfo = elektraStrDup (strLookupInfo);

		char * prevStart = dupStrLookupInfo;
		for (char * it = dupStrLookupInfo; it && *it; ++it)
		{
			if (isspace (*it))
			{
				*it = '\0';
				if (elektraStrCmp (prevStart, infoProvides))
				{
					ksDel (ksPluginConfig);
					elektraFree (dupStrLookupInfo);
					freeStrArr (allPlugins);
					return true;
				}
				prevStart = it + 1;
			}
		}
	}

	return false;
}


void (*getFuncFromKey (Key * k))(void)
{
	union
	{
		void (*f) (void);
		void * v;
	} conversation;

	/* TODO: Replace if with assert */
	if (sizeof (conversation) != sizeof (void (*) (void)))
	{
		fprintf (stderr, "union does not have size of function pointer\n");
	}

	if (keyGetBinary (k, &conversation.v, sizeof (conversation)) != sizeof (conversation))
	{
		/* TODO: Handle error */
		fprintf (stderr, "Key type mismatch\n");
	}

	return conversation.f;
}


/* Populates the KeySet `ksInfo` with a call to kdbGet() */
enum PluginStatus getPluginStatus (struct PluginSpec spec, KeySet * modules, KeySet ** ksInfo, KeySet ** ksSymbols)
{
	ksAppendKey (spec.config, keyNew ("system:/module", KEY_VALUE, "this plugin was loaded for the status", KEY_END));
	Key * errorKey;
	Plugin * plugin = elektraPluginOpen (spec.name, modules, spec.config, errorKey);

	if (plugin)
	{
		/* plugin->name might be different for default plugins */
		if (strcmp (spec.name, plugin->name) != 0)
		{
			/* save virtual name as refname */
			validatePluginName (spec.name);
			spec.refname = spec.name;

			/* use actual name */
			validatePluginName (plugin->name);
			spec.name = plugin->name;
		}

		/* loadInfo () */
		Key * infoKey = keyNew ("system:/elektra/modules", KEY_END);
		keyAddBaseName (infoKey, spec.name);

		if (!plugin->kdbGet)
		{
			//throw MissingSymbol ("kdbGet", plugin->name);
		}
		else
		{
			*ksInfo = ksNew (0, KS_END);
			plugin->kdbGet (plugin, *ksInfo, infoKey);

			/* parse () */
			Key * k = ksLookup (*ksInfo, infoKey, KDB_O_NONE);
			if (!k)
			{
				//throw PluginNoContract ();
			}

			keyAddBaseName (infoKey, "exports");

			ssize_t it = ksSearch (*ksInfo, infoKey) + 1;
			if (it > 0)
			{
				for (; it < ksGetSize (*ksInfo); ++it)
				{
					k = ksAtCursor (*ksInfo, it);
					if (keyIsBelow (infoKey, k) != 1) break;

					// C++: symbols[k.getName ().substr (root.getName ().length () + 1)] = (*k.getFunc ());
					if (!*ksSymbols)
					{
						*ksSymbols = ksNew (ksGetSize (*ksInfo), KS_END);
					}

					if (*ksSymbols)
					{
						size_t subStrStartPos = elektraStrLen (keyName (infoKey));

						if (subStrStartPos < elektraStrLen (keyName (k)))
						{
							size_t symbolKeyNameStrLen = elektraStrLen (keyName (k) + subStrStartPos);
							char * symbolKeyName = elektraMalloc (symbolKeyNameStrLen);
							strcpy (symbolKeyName, keyName (k) + subStrStartPos);

							/* name = name of symbol, value = function pointer */
							Key * keySymbol = keyNew (symbolKeyName, KEY_VALUE, getFuncFromKey (k), KEY_END);
							elektraFree (symbolKeyName);
							ksAppendKey (*ksSymbols, keySymbol);
						}
						else
						{
							/* TODO: handle error */
						}
					}


				}
			}

			keySetBaseName (infoKey, "infos");
			it = ksSearch (*ksInfo, infoKey) + 1;
			if (it > 0)
			{
				/* TODO: move ksInfos definition outside if needed at other place */
				KeySet * ksInfos = ksNew (0, KS_END);
				for (; it < ksGetSize (*ksInfo); ++it)
				{
					k = ksAtCursor (*ksInfo, it);
					if (keyIsBelow (infoKey, k) != 1) break;
					// C++: infos[k.getName ().substr (root.getName ().length () + 1)] = k.getString ();
					size_t subStrStartPos = elektraStrLen (keyName (infoKey));

					if (subStrStartPos < elektraStrLen (keyName (k)))
					{
						size_t infoKeyNameStrLen = elektraStrLen (keyName (k) + subStrStartPos);
						char * infoKeyName = elektraMalloc (infoKeyNameStrLen);
						strcpy (infoKeyName, keyName (k) + subStrStartPos);

						/* name = name of info, value = content (text) of info */
						Key * keyInfo = keyNew (infoKeyName, KEY_VALUE, keyString (k), KEY_END);
						elektraFree (infoKeyName);
						ksAppendKey (ksInfos, keyInfo);
					}
					else
					{
						/* TODO: Handle error */
					}
				}
				ksDel (ksInfos);
				ksDel (*ksInfo);
				keyDel (infoKey);
				return real;
			}
			else
			{
				// throw PluginNoInfo ();
			}

			if (hasProvides (*ksInfo, spec.name))
			{
				return provides;
			}
		}
	}

	return missing;
}


struct PluginSpec lookupProvides (char * const pluginName, KeySet **ksSymbols)
{
	/* from modules.hpp */
	KeySet * modules;

	/* 2nd parameter is unused in function implementation, therefore we use NULL here */
	elektraModulesInit (modules, NULL);

	/* Check if plugin with provider name exists */
	struct PluginSpec ps;
	setPluginFullName (&ps, pluginName);
	ps.config = ksNew (0, KS_END);

	KeySet * ksInfo;
	if (getPluginStatus (ps, modules, &ksInfo, ksSymbols) == real)
	{
		return ps;
	}

	/* C++: lookupAllProvidesWithStatus (pluginName) */
	KeySet * foundPlugins = lookupAllProvidesWithStatus (pluginName, ksInfo);

	/* Determine the plugin with the highest rank */

	long maxPoints = LONG_MIN;
	Key * keyMaxPoints = NULL;
	for (ssize_t it = 0; it < ksGetSize (foundPlugins); ++it)
	{
		Key * keyCur = ksAtCursor (foundPlugins, it);
		long curPoints = *(long *) keyValue (keyCur);

		if (curPoints > maxPoints)
		{
			maxPoints = curPoints;
			keyMaxPoints = keyCur;
		}
	}

	if (keyMaxPoints)
	{
		char * strPluginFullName = elektraMalloc (keyGetNameSize (keyMaxPoints));
		keyGetName (keyMaxPoints, strPluginFullName, keyGetNameSize (keyMaxPoints));
		setPluginFullName (&ps, strPluginFullName);
	}

	ksDel (foundPlugins);

	return ps;
}


void pluginLoadInfo (struct PluginSpec const spec)
{
	Key * infoKey = keyNew ("system:/elektra/modules", KEY_END);
	keyAddBaseName (infoKey, spec.name);


}


struct PluginSpec addPlugin (const KeySet * const ks, struct PluginSpec ps)
{
	char * pluginFullName = getPluginFullName (ps);

	if (!pluginFullName)
	{
		//return false;
	}

	for (int i = 0; i < ksGetSize (ks); i++)
	{
		Key * curKey = ksAtCursor (ks, i);
		struct PluginSpec * curPs = (struct PluginSpec *) keyValue (curKey);
		if (curPs)
		{
			char * curFullName = getPluginFullName (*curPs);
			if (strcmp (curFullName, pluginFullName) == 0)
			{
				/* TODO: Handle error */
				/* in C++: throw PluginAlreadyInserted (plugin.getFullName ()); */
				elektraFree (curFullName);
				elektraFree (pluginFullName);
				//return false;
			}
			elektraFree (curFullName);
		}
	}

	elektraFree (pluginFullName);

	/* TODO: Refactor - put check for already inserted plugins in own function */

	/* If the plugin is actually a provider use it (otherwise we will get our name back) */
	KeySet * ksSymbols;
	struct PluginSpec newPlugin = ps;
	struct PluginSpec provides = lookupProvides (ps.name, &ksSymbols);

	if (strcmp (provides.name, newPlugin.name) != 0)
	{
		/* Keep our config and refname */
		newPlugin.name = provides.name;
		ksAppend (newPlugin.config, provides.config);
	}

	/* Call the checkconf-function of the plugin (if provided)
	 * this enables a plugin to verify its configuration at mount time */
	// C++: checkConfPtr checkConfFunction = reinterpret_cast<checkConfPtr> (pluginDatabase->getSymbol (newPlugin, "checkconf"));
	int (*checkConfFunction)(Key *, KeySet*) = (int (*)(Key*, KeySet*))keyValue(ksLookupByName (ksSymbols, "checkconf", KDB_O_NONE));

	if (checkConfFunction)
	{
		Key * errorKey = keyNew ("/", KEY_END);

		/* merge plugin config and backend config together */
		KeySet * pluginConfig = ksDup (newPlugin.config);
		ksAppend (pluginConfig, ks);

		/* Call the checkconf function of the plugin */
		int checkResult = checkConfFunction (errorKey, pluginConfig);
		if (checkResult == -1)
		{
			/* TODO: Handle error */
			ksDel (pluginConfig);
			fprintf (stderr, "PluginConfigInvalid: %s", keyString (errorKey));
		}
		else if (checkResult == 1)
		{
			/* Separate plugin config from the backend config */
			Key * backendParent = keyNew ("system:/", KEY_END);
			KeySet * newBackendConfig = ksCut (pluginConfig, backendParent);

			/* Take over the new configuration */
			newPlugin.config = pluginConfig;
			// TODO: C++: setBackendConfig (modifiedBackendConfig);
			keyDel (backendParent);
			ksDel (newBackendConfig); /* TODO: remove when implemented setBackendConfig() */
		}
		else
		{
			/* TODO: Handle error */
			ksDel (pluginConfig);
		}
		keyDel (errorKey);
	}

	return newPlugin;
	//return true;

	// C++: sort()
}

/** @returns true if str1 starts with str2
 */
bool compareStrStart (const char * str1, const char * str2)
{
	if (!str2)
	{
		/* Every string starts with a NULL string */
		return true;
	}

	for (; *str1 && *str2; str1++, str2++)
	{
		if (*str1 != *str2)
			return false;
	}

	return *str2 ? false : true;
}

/* Check if dependency is relevant (occurs in KeySet) */
void checkDependencyRelevancy (const KeySet * const deps, const char * const order)
{
	for (ssize_t i = 0; i < ksGetSize (deps); i++)
	{
		Key * curKey = ksAtCursor (deps, i);
		const size_t jumpSlash = 1;
		const char * const name = keyName (curKey) + jumpSlash;

		bool hasProvides = false;
		/* The following commented out snippet was taken from the C++ Code in backendbuilder.cpp[132-144] */
		/* TODO: should also take care of provides
		 * implementation below would self-conflict on multiple same providers
			std::string provides = pluginDatabase->lookupInfo (PluginSpec(name), "provides");
			std::istringstream ss2 (provides);
			std::string provide;
			while (ss2 >> provide)
			{
				if (provide == name)
				{
					hasProvides = true;
				}
			}
		*/

		if ((elektraStrLen (name) >= elektraStrLen (order) && compareStrStart (name, order)) || hasProvides)
		{
			/* Is relevant, add this instance of dep to every other key
			 * and reverse dep of every key to curKey */
			 for (ssize_t j = 0; j < ksGetSize (deps); j++)
			 {
			 	Key * k = ksAtCursor (deps, i);
			 	if (k != curKey)
				{
					elektraMetaArrayAdd (curKey, "dep", keyName (k));
				}
			 }
		}
	}
}

/**
 * @brief Makes sure that ordering constraints are fulfilled.
 *
 * @pre a sorted list except of the last element to be inserted
 * @post the last element will be moved to a place where it does not produce an order violation
 *
 * @note its still possible that an order violation is present in the case
 *       of order violation in the other direction (no cycle detection).
 */
void sortPluginSpecArray (struct PluginSpec * pluginSpecsToAdd, size_t n, KeySet *ksInfo)
{
	KeySet * deps = ksNew (0, KS_END);

	for (size_t i = 0; i < n; i++)
	{
		const struct PluginSpec curPs = *(pluginSpecsToAdd + i);
		char * depkeyName = elektraMalloc (elektraStrLen (curPs.name) + 1);
		*depkeyName = '/';
		*(depkeyName + 1) = '\0';
		strcat (depkeyName, curPs.name);

		Key * dep = keyNew (depkeyName, KEY_END);
		elektraFree (depkeyName);

		if (elektraStrCmp (curPs.name, curPs.refname) != 0)
		{
			keyAddBaseName (dep, curPs.refname);
		}

		ksAppendKey (deps, dep);

		char depStr[10];
		if (snprintf (depStr, 10, "%zu", i) >= 10)
		{
			/* TODO: Handle overflow error */
			ksDel (deps);
			return;
		}
		keySetString (dep, depStr);
		keySetMeta (dep, "order", depStr);
	}


	KeySet * addedDeps = ksNew (0, KS_END);
	for (size_t i = 0; i < n; i++)
	{
		const struct PluginSpec curPs = *(pluginSpecsToAdd + i);
		const char * curInfo = modulesPluginDatabaseLookupInfo (curPs, ksInfo, "ordering");
		char * dupInfo = elektraStrDup (curInfo);

		char * lastStartPos = dupInfo;
		for (char * curChar = dupInfo; curChar && *curChar; curChar++)
		{
			if (isspace (*curChar))
			{
				*curChar = '\0';

				if (!ksLookupByName (addedDeps, lastStartPos, KDB_O_NONE))
				{
					ksAppendKey (addedDeps, keyNew (lastStartPos, KEY_END));
					checkDependencyRelevancy (deps, lastStartPos);
				}
				lastStartPos = curChar + 1;
			}
		}
		/* add last string */
		if (lastStartPos && *lastStartPos && !ksLookupByName (addedDeps, lastStartPos, KDB_O_NONE))
		{
			ksAppendKey (addedDeps, keyNew (lastStartPos, KEY_END));
			checkDependencyRelevancy (deps, lastStartPos);
		}
		elektraFree (dupInfo);
	}
	ksDel (addedDeps);


	/* Now sort by the given topology */
	Key ** keyArray = elektraMalloc(ksGetSize (deps) * sizeof (Key *));
	int ret = elektraSortTopology (deps, keyArray);
	if (ret == 0)
	{
		/* TODO: Handle error */
		ksDel (deps);
		elektraFree (keyArray);
		fprintf (stderr, "CyclicOrderViolation\n");
		return;
	}
	else if (ret == -1)
	{
		/* TODO: Handle error */
		ksDel (deps);
		elektraFree (keyArray);
		fprintf (stderr, "elektraSortTopology was used wrongly");
		return;
	}

	struct PluginSpec * pluginSpecsToAddCopy = elektraMalloc (n * sizeof (struct PluginSpec));
	for (size_t i = 0; i < n; i++)
	{
		*(pluginSpecsToAddCopy + i) = *(pluginSpecsToAdd + i);
	}

	/* Now swap everything in pluginSpecsToAdd as we have ordered indizes */
	for (ssize_t i = 0; i < ksGetSize (deps); i++)
	{
		Key * curKey = *(keyArray + i);

		if (!curKey)
		{
			/* TODO: Handle error */
			elektraFree (pluginSpecsToAddCopy);
			return;
		}

		char *eptr;
		long curIndex = strtol (keyString (curKey), &eptr, 10);

		if (*eptr != '\0')
		{
			/* TODO: Handle error */
			elektraFree (pluginSpecsToAddCopy);
			return;
		}

		*(pluginSpecsToAdd + i) = *(pluginSpecsToAddCopy + curIndex);
	}

	ksDel (deps);
	elektraFree (keyArray);
	elektraFree (pluginSpecsToAddCopy);
}


void cBuildBackend (KeySet * const mountConf, const char * const mountPoint, char * pluginsConfig, bool clForce, bool clDebug, int mergeStrategy, const char * resolverName, const char * path)
{
	//TODO: Maybe directly require a key as parameter (instead of the keyname)
	Key * const keyMountpoint = keyNew (mountPoint, KEY_END);
	if (!keyMountpoint)
	{
		/* TODO: Implement error handling
		 * throw invalid_argument (mp + " is not a valid mountpoint"); */
		return;
	}

	KeySet * const ksDupMountConf = ksDup (mountConf);

	/* TODO: Strategy was "!=preserve" in cpp-code, check merging for mounting */
	if (clForce || mergeStrategy != MERGE_STRATEGY_ABORT)
	{
		Key * const cutKey = keyNew (DEFAULT_MOUNTPOINTS_PATH, KEY_END);
		keyAddBaseName (cutKey, mountPoint);
		KeySet * ksCutted = ksCut (mountConf, cutKey);
		/* We don't need the cut-out KeySet, but only the changed mountconf KeySet */
		ksDel (ksCutted);
		keyDel (cutKey);
	}

	/* in C++: backend.setMountpoint (mpk, mountConf); */
	if(!isValidMountPoint (keyMountpoint, mountConf))
	{
		/* TODO: error handling */
		return;
	}


	/* TODO: Check if basepath should be with or without '/' */
	/* in C++: backend.setBackendConfig (cl.getPluginsConfig ("system:/")); */
	const KeySet * ksBackendConfig = cParsePluginArguments (pluginsConfig, "system:/");

	if (clDebug)
	{
		printf ("Trying to load the resolver plugin %s\n", resolverName);
	}

	/* in C++: PluginSpec resolver (cl.resolver); (Constructor calls setFullName) */
	struct PluginSpec psResolver;
	if (!setPluginFullName (&psResolver, resolverName))
	{
		/* TODO: error handling */
		fprintf (stderr, "Could not set full plugin name for resolver plugin!\n");
		return;
	}

	/* in C++: backend.addPlugin (PluginSpec (resolver));
	 * -> moved to own function */
	struct PluginSpec pluginToAdd = addPlugin (ksBackendConfig, psResolver);
	/* C++ end of backend.addPlugin */

	/* C++: backend.useConfigFile (path) */

	/* C++: end of backend.useConfigFile (path) */
}


























/* The following code is not directly used for mounting, but was c++-code that was called from the c++-code for mounting */

/* Currently prints directly to stderr instead of taking an output stream as parameter
 * TODO: Implement colored output (see src/tools/kdb/ansicolors.hpp) */
void cPrintWarnings (Key * const error, const bool printVerbose, const bool printDebug)
{
	KeySet * const keyErrorMeta = ksDup (keyMeta (error));
	Key * const keyParent = keyNew ("meta:/warnings", KEY_END);
	KeySet * const ksWarnings = ksCut (keyErrorMeta, keyParent);
	ksDel (keyErrorMeta);
	keyDel (keyParent);

	/* TODO: Implement error handling */
	if (!ksWarnings || ksGetSize (ksWarnings) == 0)
	{
		if (ksWarnings)
			ksDel (ksWarnings);
		return;
	}

	/* Get number of warnings */
	const Key * const keyMetaWarnings = ksLookupByName (ksWarnings, "meta:/warnings", KDB_O_NONE);
	long cntWarnings = 0;
	if (keyMetaWarnings)
	{
		const char * const strWarningCount = keyString (keyMetaWarnings);

		/* skip leading '#' and '_' characters */
		for (size_t i = 0; i < elektraStrLen (strWarningCount) && (*(strWarningCount+i) == '#' || *(strWarningCount+i) == '-'); i++);
		/* no loop body! */

		char * eptr;
		cntWarnings = strtol (strWarningCount, &eptr, 10);

		if (*eptr)
		{
			/* String could not be fully converted to a long
			 * TODO: implement error handling */
			ksDel (ksWarnings);
			 return;
		}
	}


	/* TODO: Maybe implement special handling for 0 warnings (not present in original c++ code) */
	fprintf (stderr, "Sorry, %ld warning%s issued :(\n", cntWarnings, cntWarnings == 1 ? " was" : "s were");

	cntWarnings = 0;
	for (elektraCursor i = 0; i < ksGetSize (ksWarnings); i++)
	{
		Key * keyCur = ksAtCursor (ksWarnings, i);
		const int isDirectlyBelow = keyIsDirectlyBelow (keyParent, keyCur);
		if (isDirectlyBelow == -1)
		{
			/* TODO: Implement error handling or treat as false (TDB) */
			ksDel (ksWarnings);
			return;
		}
		else if (isDirectlyBelow == 1) /* directly below */
		{
			const char * const curKeyName = keyName (keyCur);


			const size_t lenCurKeyName = elektraStrLen (curKeyName);
			/* Subtract 1 because elektraStrLen(...) counts '\0' of both strings (two function calls),
			 * use the string "description" for calculating the size, as this is the longest subkey-name we need. */
			const size_t searchKeyNameSize = lenCurKeyName + elektraStrLen ("/description") - 1;
			char * const searchKeyName = elektraMalloc (searchKeyNameSize);
			if (searchKeyName)
			{
				strcpy (searchKeyName, curKeyName);
				strcat (searchKeyName, "/module");
				const Key * const keyModule = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);
				strcpy (searchKeyName + lenCurKeyName, "number");
				const Key * const keyNumber = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);
				strcpy (searchKeyName + lenCurKeyName, "reason");
				const Key * const keyReason = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);
				strcpy (searchKeyName + lenCurKeyName, "description");
				const Key * const keyDescription = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);

				fprintf (stderr, " %zd: Module %s issued the warning %s:\n", ++cntWarnings, keyString (keyModule),
					 keyString (keyNumber));
				fprintf (stderr, "\t%s: %s\n", keyString (keyDescription), keyString (keyReason));

				if (printVerbose)
				{
					strcpy (searchKeyName + lenCurKeyName, "mountpoint");
					const Key * const keyMountpoint = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);
					strcpy (searchKeyName + lenCurKeyName, "configfile");
					const Key * const keyConfigFile = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);

					fprintf (stderr, "\tMountpoint: %s\n", keyString (keyMountpoint));
					fprintf (stderr, "\tConfigfile: %s\n", keyString (keyConfigFile));
				}

				if (printDebug)
				{
					strcpy (searchKeyName + lenCurKeyName, "file");
					const Key * const keyFile = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);
					strcpy (searchKeyName + lenCurKeyName, "line");
					const Key * const keyLine = ksLookupByName (ksWarnings, searchKeyName, KDB_O_NONE);

					fprintf (stderr, "\tAt: %s:%s\n", keyString (keyFile), keyString (keyLine));
				}
				elektraFree (searchKeyName);
			}
		}
	}

	fflush (stderr);
	ksDel (ksWarnings);
}

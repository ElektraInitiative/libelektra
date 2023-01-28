#include <kdbmount.h>
#include <kdbhelper.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <kdbmerge.h>

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

	while (*++str)0
	{
		if ((*str < 'a' || *str > 'z') && (*str < '0' || *str > '9') && *str != '_')
			return false;
	}

	return true;
}

/**
 * @brief Set the full name (name + refname) with # or only the name
 *
 * @param ps the PluginSpec for which the name(s) should be set
 * @param str the name to set
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

	return false;
}




/** @brief Parse a string containing information to create a KeySet
 *  @param pluginArguments comma (,) to separate key=value, contains no whitespaces
 */
const KeySet * cParsePluginArguments (char * const pluginArguments, const char * const basepath)
{
	const KeySet * const ks = ksNew (0, KS_END);

	/* Read until the next '=', this should be the key name */
	size_t posEqual = strcspn (pluginArguments, "=");

	/* Temporary replace '=' with '\0' to use the current substring as parameter for a keyname */
	pluginArguments[posEqual] = 0;

	//const Key * const keyToAppend = keyNew ()

	return ks;
}




void cBuildBackend (KeySet * const mountConf, const char * const mountPoint, bool clForce, bool clDebug, int mergeStrategy, char * const resolverName)
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

	if(!isValidMountPoint (keyMountpoint, mountConf))
	{
		/* TODO: error handling */
		return;
	}

	struct PluginSpec resolver;

	if (!setPluginFullName (&resolver, resolverName))
	{
		/* TODO: error handling */
		fprintf (stderr, "Could not set full plugin name for resolver plugin!\n");
		return;
	}

	KeySet * pluginsToAdd = ksNew (0, KS_END);
	Key * resolverPluginKey = keyNew (resolver.name, KEY_END);
	keySetBinary (resolverPluginKey, &resolver, sizeof(resolver));
	ksAppendKey (pluginsToAdd, resolverPluginKey);


	if (clDebug)
	{
		printf ("Trying to load the resolver plugin %s\n", resolverName);
	}


	if (clDebug)
	{
		printf ("Trying to add default plugins\n");
	}


	keyDel (keyMountpoint);
	ksDel (ksDupMountConf);
}











void lookupProvides (const char * const pluginName)
{
	/* Check if plugin with provider name exists */

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

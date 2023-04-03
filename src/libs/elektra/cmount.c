#include <kdbhelper.h>
#include <kdbmerge.h>
#include <kdbmount.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <kdbconfig.h> // for HAVE_GLOB
#include <kdbmodule.h>
#include <kdbprivate.h> // currently needed for plugin handling (struct _Plugin)

#ifdef HAVE_GLOB
#include <glob.h>
#endif

#include <ctype.h>
#include <kdblogger.h>
#include <kdberrors.h>
#include <assert.h>



/*****************************************************
 * PART I: enums and related functions               *
 *****************************************************/

enum PluginStatus
{
	/* works not directly, but can be loaded via provides */
	provides,
	/* exists and working as given */
	real,
	/* does not exist or cannot be loaded */
	missing
};

enum PluginType
{
	pluginTypeGet,
	pluginTypeSet,
	pluginTypeError,
	pluginTypeCommit
};

const char * pluginTypeToStr (enum PluginType pluginType)
{
	switch (pluginType)
	{
	case pluginTypeError:
		return "error";
	case pluginTypeCommit:
		return "commit";
	case pluginTypeGet:
		return "get";
	case pluginTypeSet:
		return "set";
	default:
		return "unknown";
	}
}



/*****************************************************
 * PART II: structs and related functions            *
 *****************************************************/

/* Simple linked list with strings */
struct StringNode
{
	char * str;
	struct StringNode * next;
	bool shouldFreeStr;
};

struct StringNode * getNodeFromStringList (struct StringNode * startNode, const char * str)
{
	for (; startNode; startNode = startNode->next)
	{
		if (elektraStrCmp(startNode->str, str) == 0)
		{
			return startNode;
		}
	}

	/* not found */
	return NULL;
}

void freeStringList (struct StringNode * startNode)
{
	for (struct StringNode * nextNode; startNode; startNode = nextNode)
	{
		if (startNode->shouldFreeStr)
		{
			elektraFree (startNode->str);
		}
		nextNode = startNode->next;
		elektraFree (startNode);
	}
}

/* returns the newly allocated node (a new start-node gets allocated if NULL is passed as parameter)
 * if multiple strings should be added, you can use the returned node as start node for subsequent calls */
struct StringNode * addStrAtEnd (struct StringNode * startNode, char * str, bool shouldFreeString)
{
	struct StringNode * newNode = elektraMalloc (sizeof (struct StringNode));
	newNode->str = str;
	newNode->shouldFreeStr = shouldFreeString;
	newNode->next = NULL;

	if (startNode)
	{
		/* forward to last node */
		for (; startNode->next; startNode = startNode->next);
		startNode->next = newNode;
	}

	return newNode;
}

/* removes elements with the provided string from a list
 * returns the start node of the filtered list (NULL if all nodes get removed)
 * if @p removeAll ist false, only the first element that was found gets removed */
struct StringNode * removeFromStringList (struct StringNode * startNode, const char * strToRemove, bool removeAll)
{
	if (!startNode)
	{
		return NULL;
	}

	/* Check if the start node must be removed */
	for (struct StringNode * tmpNode; elektraStrCmp (startNode->str, strToRemove) == 0;)
	{
		tmpNode = startNode;
		startNode = startNode->next;
		tmpNode->next = NULL;
		freeStringList (tmpNode);

		if (!removeAll)
		{
			return startNode;
		}
	}

	/* Check other nodes */
	for (struct StringNode * prevNode = startNode, *tmpNode = startNode->next; tmpNode; prevNode = tmpNode, tmpNode = tmpNode->next)
	{
		if (elektraStrCmp (tmpNode->str, strToRemove) == 0)
		{
			prevNode->next = tmpNode->next;
			tmpNode->next = NULL;
			freeStringList (tmpNode);

			if (!removeAll)
			{
				return startNode;
			}
			else
			{
				/* prevNode stays the same as in the previous iteration */
				tmpNode = prevNode;
			}
		}
	}

	return startNode;
}

/* Be aware that the string `str` is changed by the function!
 * Returns the inserted node #
 * If you want to free the string, be sure to only free the char * of the start node! */
struct StringNode * splitAndMoveStringToStringList (struct StringNode * startNode, char * str)
{
	char * lastStart = str;
	struct StringNode * curNode = startNode;
	bool endReached = false;

	for (; str && !endReached; str++)
	{
		if (isspace (*str) || !(*str))
		{
			if (!(*str))
			{
				/* end of string reached */
				endReached = true;
			}
			else
			{
				*str = '\0';
			}

			if (!startNode)
			{
				/* Create new start node */
				startNode = addStrAtEnd (NULL, lastStart, false);
				curNode = startNode;
			}
			else
			{
				/* Use existing start node */
				curNode = addStrAtEnd (curNode, lastStart, false);
			}

			lastStart = str + 1;
		}
	}

	return curNode;
}

struct StringNode * splitAndCopyStringToStringList (struct StringNode * startNode, const char * str)
{
	const char * lastStart = str;
	struct StringNode * curNode = startNode;
	bool endReached = false;

	for (; str && !endReached; str++)
	{
		if (isspace (*str) || !(*str))
		{
			if (!(*str))
			{
				endReached = true;
			}

			char * newStr = elektraMalloc (str - lastStart + 1);
			strncpy (newStr, lastStart, str - lastStart);
			newStr[str-lastStart] = '\0';
			lastStart = str + 1;

			if (!startNode)
			{
				/* Create new start node */
				startNode = addStrAtEnd (NULL, newStr, true);
				curNode = startNode;
			}
			else
			{
				/* Use existing start node */
				curNode = addStrAtEnd (curNode, newStr, true);
			}
		}
	}

	return curNode;
}


struct BackendInfo
{
	/* Where the backend is mounted */
	const char * mountPoint;

	/* Path to file */
	const char * path;
	struct BackendInfo * next;
};

void freeBackendInfoList (struct BackendInfo * startNode)
{
	struct BackendInfo * nextNode;
	while (startNode)
	{
		nextNode = startNode->next;
		elektraFree(startNode);
		startNode = nextNode;
	}
}


/* Structs for storing specifications (name + config) for plugins */
struct PluginSpec
{
	char * name;

	/* When functions for dealing with fullNames are used, often only *name must be freed, but not *refname, as they two strings
	 * belong to the same allocated memory area! Therefore, if you set a custom allocated string to *refname,
	 * make sure to free it manually */
	char * refname;
	KeySet * config;
};

/* Can be used as a node for a linked list */
struct PluginSpecNode
{
	struct PluginSpec ps;
	struct PluginSpecNode * next;
};

void freePsList (struct PluginSpecNode * psn, bool freePluginSpecs)
{
	for (struct PluginSpecNode * nextNode; psn; psn = nextNode)
	{
		nextNode = psn->next;

		if (freePluginSpecs)
		{
			elektraFree (psn->ps.name);
			ksDel (psn->ps.config);
		}

		elektraFree (psn);
	}
}


/* List with information for a plugin */
struct PluginInfos
{
	const char * infoName;
	const char * infoVal;
	struct PluginInfos * next;
};

/* Returns the newly allocated node
 * If NULL is passed for @p startNode , a new List is created and returned */
struct PluginInfos * addInfoToInfoList (struct PluginInfos * startNode, const char * infoName, const char * infoVal)
{
	struct PluginInfos * newNode = elektraMalloc (sizeof (struct PluginInfos));
	newNode->infoName = infoName;
	newNode->infoVal = infoVal;
	newNode->next = NULL;

	if (startNode)
	{
		/* forward to last node */
		for (; startNode->next; startNode = startNode->next);
		startNode->next = newNode;
	}

	return newNode;
}

/* List of symbols (functions) for a plugin */
struct PluginSymbols
{
	const char * symbolName;
	void (* symbolFunc) (void);
	struct PluginSymbols * next;
};

/* Returns the newly allocated node
 * If NULL is passed for startNode, a new List is created and returned */
struct PluginSymbols * addFuncToSymbolList (struct PluginSymbols * startNode, const char * symbolName, void (* symbolFunc) (void))
{
	struct PluginSymbols * newNode = elektraMalloc (sizeof (struct PluginSymbols));
	newNode->symbolName = symbolName;
	newNode->symbolFunc = symbolFunc;
	newNode->next = NULL;

	if (startNode)
	{
		/* forward to last node */
		for (; startNode->next; startNode = startNode->next);
		startNode->next = newNode;
	}

	return newNode;
}

void (*getSymbolFromList (struct PluginSymbols * startNode, const char * symbolName)) (void)
{
	for (; startNode; startNode = startNode->next)
	{
		if (elektraStrCmp (startNode->symbolName, symbolName) == 0)
		{
			return startNode->symbolFunc;
		}
	}

	return NULL;
}

struct Plugin
{
	/* the low-level representation of a plugin inside Elektra */
	struct _Plugin * plugin;

	KeySet * ksInfo;
	struct PluginSpec ps;

	/* The following two lists can contain pointers that are taken from keys in ksInfo!
	 * So make sure to not ksDel() ksInfo before the last use of these lists. */
	struct PluginSymbols * symbols;
	struct PluginInfos * infos;
};

/* Can be used as a node for a list of plugins */
struct PluginNode
{
	struct Plugin plugin;
	struct PluginNode * next;
};

void freePlugin(bool closePlugin, struct Plugin plugin, struct PluginSpec cmp)
{
	if (closePlugin)
	{
		elektraPluginClose (plugin.plugin, NULL);
	}

	if (plugin.ps.name != cmp.name)
	{
		elektraFree (plugin.ps.name);
	}

	/* The refname in ps, if not equal to cmp.refname, must have its own allocated memory */
	if (plugin.ps.refname != plugin.ps.name && plugin.ps.refname != cmp.refname)
	{
		elektraFree (plugin.ps.refname);
	}

	if (plugin.ps.config != cmp.config)
	{
		ksDel (plugin.ps.config);
	}

	while (plugin.symbols)
	{
		struct PluginSymbols * nextNode = plugin.symbols->next;
		elektraFree (plugin.symbols);
		plugin.symbols = nextNode;
	}

	while (plugin.infos)
	{
		struct PluginInfos * nextNode = plugin.infos->next;
		elektraFree (plugin.infos);
		plugin.infos = nextNode;
	}

	ksDel (plugin.ksInfo);
}

/* A collection of plugins (either get, set, commit or error) */
/* TODO: Check if slot could be defined for all plugins (move char * slot to outer struct) */
struct Plugins
{
	struct Plugins_PluginList
	{
		const char * slot;
		struct PluginNode * plugins;
		struct Plugins_PluginList * next;
	} * pluginList;

	struct StringNode * needed;
	struct StringNode * recommended;
	struct StringNode * alreadyProvided;
	struct StringNode * alreadyConflict;

	int nrStoragePlugins;
	int nrResolverPlugins;
};

/* @p freePluginsStruct if the given pointer itself should be freed (don't use it after calling this function!) */
void freePlugins (struct Plugins * toFree, bool freePluginsStruct)
{
	freeStringList (toFree->needed);
	freeStringList (toFree->recommended);
	freeStringList (toFree->alreadyProvided);
	freeStringList (toFree->alreadyConflict);

	while (toFree->pluginList)
	{
		while (toFree->pluginList->plugins)
		{
			/* Plugins inside this PluginNodes should not be freed here, because they are just a flat copy */
			struct PluginNode * nextNode = toFree->pluginList->plugins->next;
			elektraFree (toFree->pluginList->plugins);
			toFree->pluginList->plugins = nextNode;
		}

		struct Plugins_PluginList * nextNode = toFree->pluginList->next;
		elektraFree (toFree->pluginList);
		toFree->pluginList = nextNode;
	}

	if (freePluginsStruct)
	{
		elektraFree (toFree);
	}
}


/* For rating plugins (select best suited plugin) */
struct PluginScore
{
	char * name;
	int points;
};

/* Assign scores to plugin-specifications */
struct PluginSpecScoreNode
{
	struct PluginSpec ps;
	int points;
	struct PluginSpecScoreNode * next;
};

struct PluginSpecScoreNode * appendPluginScore (struct PluginSpecScoreNode * listStart, struct PluginSpec ps, int points)
{
	if (!listStart)
	{
		listStart = elektraMalloc (sizeof (struct PluginSpecScoreNode));
	}
	else
	{
		for (; listStart->next; listStart = listStart->next);
		listStart->next = elektraMalloc (sizeof (struct PluginSpecScoreNode));
		listStart = listStart->next;
	}

	listStart->ps = ps;
	listStart->points = points;
	listStart->next = NULL;

	return listStart;
}

void freePluginSpecScoreList (struct PluginSpecScoreNode * listStart, bool freePluginSpecs)
{
	if (!listStart)
	{
		return;
	}
	else
	{
		freePluginSpecScoreList (listStart->next, freePluginSpecs);

		if (freePluginSpecs)
		{
			elektraFree (listStart->ps.name);
			ksDel (listStart->ps.config);
		}

		elektraFree (listStart);
	}
}



/*****************************************************
 * PART III: General (helper) functions              *
 *****************************************************/

/* Compare function for sorting strings */
int pstrcmp (const void * a, const void * b)
{
	char * const * pa = a;
	char * const * pb = b;
	return elektraStrCmp (*pa, *pb);
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
	else if (!str1)
	{
		/* A NULL-string starts only with a NULL-string */
		return false;
	}
	else
	{
		for (; *str1 && *str2; str1++, str2++)
		{
			if (*str1 != *str2)
			{
				return false;
			}
		}
	}

	return *str2 ? false : true;
}


/* The provided array must have NULL as the last element (no NULL before an element, no element after a NULL) */
void freeStrArr (char ** strArr, bool freeStrings)
{
	if (freeStrings)
	{
		for (char ** curStr = strArr; curStr; curStr++)
		{
			elektraFree (*curStr);
		}
	}
	else
	{
		/* A continuous memory section was used for all strings in the string array, so we just have to free *strArr and strArr */
		elektraFree (*strArr);
	}

	elektraFree (strArr);
}


void (*getFuncFromKey (Key * k)) (void)
{
	union
	{
		void (*f) (void);
		void * v;
	} conversation;

	assert (sizeof (conversation) == sizeof (void (*) (void)));

	if (keyGetBinary (k, &conversation.v, sizeof (conversation)) != sizeof (conversation))
	{
		/* TODO: Handle error */
		fprintf (stderr, "Key type mismatch\n");
		return NULL;
	}
	else
	{
		return conversation.f;
	}
}

// C++: from backends.cpp[138-144]
/* make sure to free the returned buffer! */
char * getBasePath (const char * mp)
{
	Key * k = keyNew (DEFAULT_MOUNTPOINTS_PATH, KEY_END);

	/* canonify name */
	Key * kmp = keyNew (mp, KEY_END);

	/* escape name */
	keyAddBaseName (k, keyName(kmp));
	keyDel (kmp);

	char * outBuf = elektraMalloc (keyGetNameSize (k));
	if (keyGetName (k, outBuf, keyGetNameSize (k)) > 0)
	{
		keyDel (k);
		return outBuf;
	}
	else
	{
		/* TODO: Handle error */
		elektraFree (outBuf);
		keyDel(k);
		return NULL;
	}
}



/*****************************************************
 * PART IV: functions for mounting                  *
 *****************************************************/

/**
 * @brief give info about current mounted backends
 *
 * @param mountConf a keyset that contains everything below
 * DEFAULT_MOUNTPOINTS_PATH
 *
 * @return A list with nodes that contain information about mounted backends.
 * The strings are only referenced in the returned list, so be sure that @p ksMountConf remains valid as long as you access strings
 * in the returned list!
 */
// C++: backends.cpp[28-50]
struct BackendInfo * getBackendInfo (KeySet * ksMountConf)
{
	if (!ksMountConf || ksGetSize (ksMountConf) <= 0)
	{
		return NULL;
	}

	Key * rootKey = keyNew (DEFAULT_MOUNTPOINTS_PATH, KEY_END);

	struct BackendInfo * resultList = NULL;
	struct BackendInfo * curNode;

	for (elektraCursor it = 0; it < ksGetSize (ksMountConf); it++)
	{
		Key * cur = ksAtCursor (ksMountConf, it);
		if (keyIsDirectlyBelow (rootKey, cur))
		{
			size_t lenLookup = keyGetNameSize (cur); /* includes \0 */
			if (lenLookup > 1)
			{
				lenLookup += elektraStrLen ("/definition/path") - 1;
				char * strLookup = elektraMalloc (lenLookup);
				strcpy (strLookup, keyName (cur));
				strcat (strLookup, "/definition/path");
				Key * path = ksLookupByName (ksMountConf, strLookup, KDB_O_NONE);
				elektraFree (strLookup);


				if (!resultList)
				{
					/* Create new list */
					resultList = elektraMalloc (sizeof (struct BackendInfo));
					resultList->next = NULL;
					curNode = resultList;
				}
				else
				{
					/* Add new node to list */
					curNode->next = elektraMalloc (sizeof (struct BackendInfo));
					curNode = curNode->next;
					curNode->next = NULL;
				}

				if (path)
				{
					/* value of key = path */
					/* copy only the pointer, the actual string is still stored inside @p ksMountConf ! */
					curNode->path = keyString (path);
				}
				else
				{
					curNode->path = NULL;
				}

				/* copy only the pointer, the actual string is still stored inside @p ksMountConf ! */
				curNode->mountPoint = keyBaseName (cur);
			}
			else
			{
				/* TODO: handle error */
				fprintf (stderr, "A key with no name was found in ksMountConf while getting backend info.!\n");
			}
		}
	}

	keyDel (rootKey);
	return resultList;
}

/* Print the currently mounted resources (mounting table) */
void cOutputMtab (KeySet * ksMountConf, bool clFirst, bool clSecond, bool clNull)
{
	// in c++: Vector with BackendInfo-structs
	struct BackendInfo * mtab = getBackendInfo (ksMountConf);
	char delim = clNull ? '\0' : '\n';

	while (mtab)
	{
		if (!clFirst)
		{
			printf ("%s", mtab->path);
			if (!clSecond)
			{
				printf (" on ");
			}
			else
			{
				printf ("%s%c", mtab->mountPoint, delim);
			}
		}

		if (!clSecond)
		{
			printf ("%s%c", mtab->mountPoint, delim);
		}

		/* remove first node from list and forward to next node */
		struct BackendInfo * nextNode = mtab->next;
		elektraFree (mtab);
		mtab = nextNode;
	}
}

/* Read mount configuration from the KDB
 * Make sure to ksDel(...) the returned KeySet */
// C++: MountBaseCommand::readMountConf in mountbase.cpp[31-41]
KeySet * getMountConfig (KDB * handle, const char * const mountpointsPath)
{
	Key * parent = NULL;

	if (!mountpointsPath || !(*mountpointsPath))
		parent = keyNew (DEFAULT_MOUNTPOINTS_PATH, KEY_END);
	else
		parent = keyNew (mountpointsPath, KEY_END);

	KeySet * mountInfo = ksNew (0, KS_END);
	if (kdbGet (handle, mountInfo, parent) == -1)
	{
		/* TODO: Implement error handling */
		fprintf (stderr, "Could not get mount config (kdbGet returned error)!\n");
		keyDel (parent);
		ksDel (mountInfo);
		return NULL;
	}

	keyDel (parent);
	return mountInfo;
}


// C++: void Backend::setMountpoint (Key mountpoint, KeySet mountConf) in backend.cpp[94-229]
bool isValidMountPoint (Key * keyMountPoint, KeySet * ksMountConf)
{
	/* STEP 0: Check for null key */
	if (!keyMountPoint)
	{
		// TODO: set error key (was exception in c++ code)
		fprintf (stderr, "Null mountpoint not allowed!\n");
		return false;
	}


	/* STEP 1: Check for empty name */
	if (keyGetNameSize (keyMountPoint) < 2) /* \0 is counted --> len >= 2 for non-empty string */
	{
		// TODO: set error key (was exception in c++ code)
		fprintf (stderr, "Empty mountpoint not allowed!\n");
		return false;
	}

	/* STEP 2: Check for wrong namespace (proc) */
	if (keyGetNamespace (keyMountPoint) == KEY_NS_PROC)
	{
		// TODO: proc-mounts should be allowed in the future */
		fprintf (stderr, "proc:/ mountpoint not allowed!\n");
		return false;
	}


	/* STEP 3: Check for name match */

	struct BackendInfo * backendInfos = getBackendInfo (ksMountConf);
	struct StringNode * alreadyUsedMountpoints = NULL;
	struct StringNode * curAlreadyUsedMountpoint = NULL;

	/* Handle cascading mountpoints (multiple namespaces) */
	for (struct BackendInfo * curNode = backendInfos; curNode; curNode = curNode->next)
	{
		if (curNode->mountPoint && *(curNode->mountPoint) == '/')
		{
			if (elektraStrLen (curNode->mountPoint) == 2)
			{
				if (!alreadyUsedMountpoints)
				{
					assert (!curAlreadyUsedMountpoint);
					alreadyUsedMountpoints = addStrAtEnd (NULL, "spec:/", false);
					curAlreadyUsedMountpoint = alreadyUsedMountpoints;
				}
				else
				{
					assert (curAlreadyUsedMountpoint);
					curAlreadyUsedMountpoint = addStrAtEnd (curAlreadyUsedMountpoint, "spec:/", false);
				}

				curAlreadyUsedMountpoint = addStrAtEnd (curAlreadyUsedMountpoint, "dir:/", false);
				curAlreadyUsedMountpoint = addStrAtEnd (curAlreadyUsedMountpoint, "user:/", false);
				curAlreadyUsedMountpoint = addStrAtEnd (curAlreadyUsedMountpoint, "system:/", false);
			}
			else
			{
				char * tmpStr = elektraMalloc (elektraStrLen ("dir") + elektraStrLen (curNode->mountPoint));
				strcpy (tmpStr, "dir:");
				strcat (tmpStr, curNode->mountPoint);

				if (!alreadyUsedMountpoints)
				{
					assert (!curAlreadyUsedMountpoint);
					alreadyUsedMountpoints = addStrAtEnd (NULL, tmpStr, true);
					curAlreadyUsedMountpoint = alreadyUsedMountpoints;
				}
				else
				{
					assert (curAlreadyUsedMountpoint);
					curAlreadyUsedMountpoint = addStrAtEnd (curAlreadyUsedMountpoint, tmpStr, true);
				}

				tmpStr = elektraMalloc (elektraStrLen ("user") + elektraStrLen (curNode->mountPoint));
				strcpy (tmpStr, "user:");
				strcat (tmpStr, curNode->mountPoint);
				curAlreadyUsedMountpoint = addStrAtEnd (curAlreadyUsedMountpoint, tmpStr, true);

				tmpStr = elektraMalloc (elektraStrLen ("system") + elektraStrLen (curNode->mountPoint));
				strcpy (tmpStr, "system:");
				strcat (tmpStr, curNode->mountPoint);
				curAlreadyUsedMountpoint = addStrAtEnd (curAlreadyUsedMountpoint, tmpStr, true);
			}
		}

		/* always add name itself, too */
		if (!alreadyUsedMountpoints)
		{
			assert (!curAlreadyUsedMountpoint);
			alreadyUsedMountpoints = addStrAtEnd (NULL, (char *) curNode->mountPoint, false);
			curAlreadyUsedMountpoint = alreadyUsedMountpoints;
		}
		else
		{
			assert (curAlreadyUsedMountpoint);
			/* The struct BackendInfo * backendInfos can be freed, as long as ksMountConf, where the string is actually stored,
			 * remains valid */
			curAlreadyUsedMountpoint = addStrAtEnd (curAlreadyUsedMountpoint, (char *) curNode->mountPoint, false);
		}
	}
	freeBackendInfoList (backendInfos);


	const char * mpName = keyName (keyMountPoint);

	/* Check for violations */
	bool violationFound = false;

	if (mpName && *mpName == '/')
	{
		if (keyGetNameSize (keyMountPoint) == 2)
		{
			if (getNodeFromStringList (alreadyUsedMountpoints, "/"))
			{
				fprintf (stderr, "Root mountpoint not possible, because the root mountpoint already exists.\n");
				violationFound = true;
			}

			/* checks below should compare against the keyname */
			Key * keyCmp = keyNew ("spec:/", KEY_END);
			if (getNodeFromStringList (alreadyUsedMountpoints, keyName (keyCmp)))
			{
				fprintf (stderr, "Root mountpoint not possible, because spec mountpoint already exists.\n");
				violationFound = true;
			}

			keySetName (keyCmp, "dir:/");
			if (getNodeFromStringList (alreadyUsedMountpoints, keyName (keyCmp)))
			{
				fprintf (stderr, "Root mountpoint not possible, because dir mountpoint already exists.\n");
				violationFound = true;
			}

			keySetName (keyCmp, "user:/");
			if (getNodeFromStringList (alreadyUsedMountpoints, keyName (keyCmp)))
			{
				fprintf (stderr, "Root mountpoint not possible, because user mountpoint already exists.\n");
				violationFound = true;
			}

			keySetName (keyCmp, "system:/");
			if (getNodeFromStringList (alreadyUsedMountpoints, keyName (keyCmp)))
			{
				fprintf (stderr, "Root mountpoint not possible, because system mountpoint already exists.\n");
				violationFound = true;
			}

			keyDel (keyCmp);
		}
		else
		{
			if (getNodeFromStringList (alreadyUsedMountpoints, mpName))
			{
				fprintf (stderr, "Cascading mountpoint %s not possible, because cascading mountpoint %s already exists.\n", mpName, mpName);
				violationFound = true;
			}

			char * tmpStr = elektraMalloc (elektraStrLen (mpName) + elektraStrLen ("system"));
			strcpy (tmpStr, "dir:");
			strcat (tmpStr, mpName);

			Key * keyCmp = keyNew (tmpStr, KEY_END);
			if (getNodeFromStringList (alreadyUsedMountpoints, keyName (keyCmp)))
			{
				fprintf (stderr, "Cascading mountpoint %s not possible, because dir mountpoint already exists.\n", mpName);
				violationFound = true;
			}

			strcpy (tmpStr, "user:");
			strcat (tmpStr, mpName);
			keySetName (keyCmp, tmpStr);
			if (getNodeFromStringList (alreadyUsedMountpoints, keyName (keyCmp)))
			{
				fprintf (stderr, "Cascading mountpoint %s not possible, because user mountpoint already exists.\n", mpName);
				violationFound = true;
			}

			strcpy (tmpStr, "system:");
			strcat (tmpStr, mpName);
			keySetName (keyCmp, tmpStr);
			if (getNodeFromStringList (alreadyUsedMountpoints, keyName (keyCmp)))
			{
				fprintf (stderr, "Cascading mountpoint %s not possible, because system mountpoint already exists.\n", mpName);
				violationFound = true;
			}

			keyDel (keyCmp);
			elektraFree (tmpStr);
		}

		if (violationFound)
		{
			freeStringList (alreadyUsedMountpoints);
			return false;
		}
	}
	else
	{
		Key * keyCmp = keyNew (mpName, KEY_END);
		if (keyCmp)
		{
			if (getNodeFromStringList (alreadyUsedMountpoints, keyName (keyCmp)))
			{
				keyDel (keyCmp);

				fprintf (stderr, "Mountpoint %s is one of the already used names:", mpName);
				for (struct StringNode * curNode = alreadyUsedMountpoints; curNode; curNode = curNode->next)
				{
					fprintf (stderr, " %s", curNode->str);
				}
				fprintf (stderr, "\n");

				freeStringList (alreadyUsedMountpoints);
				return false;
			}
			else
			{
				keyDel (keyCmp);
			}
		}
		else
		{
			freeStringList (alreadyUsedMountpoints);
			fprintf (stderr, "Invalid mountpoint: %s\n", mpName);
			return false;
		}
	}


	/* TODO: STEP 4: Check if mounted below system:/elektra */
	Key * elektraCheck = keyDup (keyMountPoint, KEY_CP_NAME);

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
		fprintf (stderr, "Mountpoint %s is below the reserved name /elektra.\nThis is not allowed because it would cause inconsistencies in this or future versions.\n", mpName);
		violationFound = true;
	}

	keyDel (elektraKey);
	keyDel (elektraCheck);
	freeStringList (alreadyUsedMountpoints);

	/* return true if no violation was found */
	return !violationFound;
}




/*****************************************************
 * PART V: Plugin-related stuff                     *
 *****************************************************/

/* from plugindatabase.cpp[52-98] */
/* For the returned string-array, make sure to free the individual strings AND the array that holds the char*-pointers
 * [out] freeStrings tells the caller if the strings in the returned array have to be freed individually or just the first-string
 * (continuous memory was allocated for all strings in the array) */
char ** getAllPluginNames (bool * freeStrings)
{
	if (!freeStrings)
	{
		fprintf (stderr, "NULL pointer given for argument 'freeStrings' in function 'getAllPluginNames'\n");
		return NULL;
	}

	char ** ret = NULL; /* Last element must be NULL */
	size_t retIndex = 0;

#ifdef ELEKTRA_SHARED
#ifdef HAVE_GLOB
	const char * toIgnore[] = { "proposal", "core", "ease", "meta", "plugin", "full", "kdb", "static", NULL };
	glob_t pglob;

	if (glob (BUILTIN_PLUGIN_FOLDER "/libelektra-*", GLOB_NOSORT, NULL, &pglob) == 0)
	{
		ELEKTRA_LOG ("has glob %zd", pglob.gl_pathc);

		/* iterate over matched pathnames */
		ret = elektraMalloc ((pglob.gl_pathc + 1) * sizeof (char *));
		for (size_t i = 0; i < pglob.gl_pathc; i++)
		{
			char * curPathname = pglob.gl_pathv[i];
			/* find last '-' in string */
			size_t startPos = -1;
			for (size_t j = 0; curPathname[j] != '\0'; j++)
			{
				if (curPathname[j] == '-')
				{
					startPos = j;
				}
			}

			if (startPos == (size_t) (-1))
			{
				/* ignore wrong file */
				continue;
			}

			/* find first '.' after the last '-' */
			size_t endPos;
			for (endPos = startPos; curPathname[endPos + 1] != '\0' && curPathname[endPos + 1] != '.'; endPos++);

			if (endPos > startPos && curPathname[endPos + 1] == '.')
			{
				endPos++;
			}
			else
			{
				/* ignore wrong file */
				continue;
			}

			/* Copy substring (start after last '-' and end before the next '.') */
			assert ((endPos - startPos) > 1);
			char * extractedName = elektraMalloc ((endPos - startPos) * sizeof (char));

			for (size_t j = startPos + 1; j < endPos; j++)
			{
				extractedName[j - startPos - 1] = curPathname[j];
			}

			extractedName[endPos - startPos - 1] = '\0';

			/* check if the entry should be ignored */
			bool ignoreEntry = false;
			for (i = 0; *(toIgnore + i); i++)
			{
				if (elektraStrCmp (extractedName, *(toIgnore + i)) == 0)
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


	if (retIndex > 0)
	{
		/* C++: std::sort (ret.begin (), ret.end ()); */
		qsort (*ret, retIndex, sizeof (char *), pstrcmp);
		*freeStrings = true;
		return ret;
	}
	else
	{
		elektraFree (ret);
	}
#endif /* HAVE_GLOB */
#endif /* ELEKTRA_SHARED */

/* if we did not find plugins, return builtinPlugins
   (even if they might be wrong for ELEKTRA_SHARED) */

	/* count number of plugins */
	size_t numPlugins = 0;
	for (size_t i = 0; i < elektraStrLen (ELEKTRA_PLUGINS); i++)
	{
		if (i == 1) /* non-empty string --> at least one plugin */
			numPlugins = 1;

		if (ELEKTRA_PLUGINS[i] == ';')
			numPlugins++;
	}

	ret = elektraMalloc ((numPlugins + 1) * sizeof (char *));

	char * builtinPlugins = elektraStrDup (ELEKTRA_PLUGINS);

	if (builtinPlugins && *builtinPlugins)
	{
		retIndex = 1;
		ret[0] = builtinPlugins;
		for (size_t i = 0; builtinPlugins[i] != '\0'; i++)
		{
			if (builtinPlugins[i] == ';')
			{
				builtinPlugins[i] = '\0';
				assert (retIndex < numPlugins);
				ret[retIndex++] = builtinPlugins + i + 1;
			}
		}
		ret[retIndex] = NULL; /* last element */
	}
	else
	{
		/* TODO: handle error */
		elektraFree (builtinPlugins);
		elektraFree (ret);
		return NULL;
	}

	/* sort all strings in the array, except the last one, which is the ending NULL */
	qsort (ret, retIndex, sizeof (char *), pstrcmp);

	/* Remove duplicates */
	for (size_t i = 1; i < retIndex; i++)
	{
		if (elektraStrCmp (ret[i - 1], ret[i]) == 0)
		{
			/* remove duplicate and move subsequent items one step up */
			retIndex--;
			for (size_t j = i; j < retIndex; j++)
			{
				/* No freeing here, because the memory for all strings in ret must be freed at once
				 * by calling elektraFree (*ret) and then elektraFree (ret) to free the array with char pointers */
				ret[j] = ret[j + 1];
			}
			ret[retIndex] = NULL; /* remove last element */
		}
	}


	/* TODO: resize ret when optimizing for small size */
	*freeStrings = false;
	return ret;
}

int calculatePluginScore (char * strStatus)
{
	/* TODO: Directly use data from CONTRACT.INI */
	// clang-format off

	if (!strStatus || !(*strStatus))
	{
		fprintf (stderr, "NULL or empty string given for strStatus!\n");
		return INT_MIN;
	}

	const struct PluginScore pointMap[31] =
	{
		{"default",      64000},
		{"recommended",  32000},
		{"productive",    8000},
		{"maintained",    4000},
		{"reviewed",      4000},
		{"conformant",    2000},
		{"compatible",    2000},
		{"coverage",      2000},
		{"specific",      1000},

		{"unittest",      1000},
		{"shelltest",     1000},
		{"tested",         500},
		{"nodep",          250},
		{"libc",           250},
		{"configurable",    50},
		{"final",           50},
		{"global",           1},
		{"readonly",         0},
		{"writeonly",        0},
		{"preview",        -50},
		{"memleak",       -250},
		{"experimental",  -500},
		{"difficult",     -500},
		{"limited",       -750},
		{"unfinished",   -1000},
		{"old",          -1000},
		{"nodoc",        -1000},
		{"concept",      -2000},
		{"orphan",       -4000},
		{"obsolete",     -4000},
		{"discouraged", -32000}
	};


	int ret = 0;

	for (char * strPrevStart = strStatus; *strPrevStart; strStatus++)
	{
		if (!(*strStatus) || isspace (*strStatus))
		{
			bool lastIteration = *strStatus ? false : true;
			*strStatus = '\0';

			int i;
			for (i = 0; i < 31; i++)
			{
				if (elektraStrCmp (pointMap[i].name, strPrevStart) == 0)
				{
					ret += pointMap[i].points;
					break;
				}
			}

			if (i == 31)
			{
				/* Status not found in map */

				char * eptr;
				long numStatus = strtol (strPrevStart, &eptr, 10);

				if ((numStatus + ret) > INT_MAX || (numStatus + ret) < (INT_MIN + 1))
				{
					fprintf (stderr, "Invalid argument for status given, integer overflow occurred: %s\n",
						strPrevStart);
				}
				else if (!(*eptr))
				{
					ret += (int) numStatus;
				}
				else
				{
					fprintf (stderr, "Invalid argument for status given: %s\n", strPrevStart);
				}
			}

			if (lastIteration)
			{
				break;
			}
			else
			{
				strPrevStart = strStatus + 1;
			}
		}
	}

	return ret;
}



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
	if (!str || !(*str))
	{
		return false;
	}

	/* must start with a-z */
	if (*str < 'a' || *str > 'z')
	{
		return false;
	}

	/* only a-z, 0-9 and '_' allowed */
	while (*(++str))
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
 * @param str the name to set, must be valid as long as @p ps is in use (caller is responsible for freeing)
 *
 * @return true if the str was valid, false otherwise
 */
bool setPluginFullName (struct PluginSpec * ps, char * str)
{
	if (validatePluginName (str))
	{
		for (char * it = str; *it; it++)
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
		ps->refname = NULL;
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
	if ((!ps.name || !*(ps.name)) && (!ps.refname || !*(ps.refname)))
	{
		return NULL;
	}

	/* count \0 for both strings, use one byte for the "#" char (if refname is present) */
	char * result = elektraMalloc ((ps.name ? elektraStrLen (ps.name) : 1) + (ps.refname ? elektraStrLen (ps.refname) : 1));
	*result = '\0';

	if (ps.name && *(ps.name))
	{
		strcpy (result, ps.name);
	}

	if (ps.refname && *(ps.refname))
	{
		strcat (result, "#");
		strcat (result, ps.refname);
	}

	return result;
}

/* The returned plugin takes ownership of ps (esp. of the pointers inside the struct)! */
// C++: PluginPtr Modules::load (PluginSpec const & spec) in modules.cpp[47-54]
struct Plugin loadPluginFromSpec (struct PluginSpec _ps, KeySet * ksModules, bool * errOccurred)
{
	struct Plugin p;
	p.ps = _ps;
	p.ksInfo = NULL;
	p.infos = NULL;
	p.symbols = NULL;

	/* TODO: replace NULL with errorKey for error handling */
	/* TODO: Check if ksDup is needed here */
	p.plugin = elektraPluginOpen (p.ps.name, ksModules, ksDup (p.ps.config), NULL);

	if (!p.plugin)
	{
		/* TODO: Handle error */
		fprintf (stderr, "No plugin could be loaded (elektraPluginOpen returned NULL)!\n");
		if (errOccurred)
		{
			*errOccurred = true;
		}

		return p;
	}

	bool freePsStrings = false;
	if (elektraStrCmp (p.ps.name, p.plugin->name) != 0)
	{
		/* save virtual name as refname */
		//char * tmp = p.ps.refname;
		p.ps.refname = getPluginFullName (p.ps);

		/* TODO: Check if freeing of refname is really the right thing to do here! */
		//elektraFree (tmp);

		/* use actual name */
		p.ps.name = elektraStrDup (p.plugin->name);

		freePsStrings = true;
	}


	// C++: from Plugin::loadInfo() (plugin.cpp[96-112])
	/* TODO: Define constant instead of using string literal here */
	Key * keyInfo = keyNew ("system:/elektra/modules", KEY_END);
	keyAddBaseName (keyInfo, p.ps.name);

	if (p.plugin->kdbGet)
	{
		p.ksInfo = ksNew (0, KS_END);
		p.plugin->kdbGet (p.plugin, p.ksInfo, keyInfo);
	}
	else
	{
		// C++: throw MissingSymbol ("kdbGet", plugin->name);
		fprintf (stderr,"The symbol 'kdbGet' is missing from the plugin: %s\n", p.plugin->name);
		if (errOccurred)
		{
			*errOccurred = true;
		}

		keyDel (keyInfo);
		elektraPluginClose (p.plugin, NULL);
		p.plugin = NULL;

		if (freePsStrings)
		{
			elektraFree (p.ps.name);
			elektraFree (p.ps.refname);
			p.ps.name = NULL;
			p.ps.refname = NULL;
		}

		return p;
	}

	// C++: from Plugin::parse() (plugin.cpp[115-155])
	Key * k = ksLookup (p.ksInfo, keyInfo, KDB_O_NONE);

	if (!k)
	{
		// C++: throw PluginNoContract ();
		fprintf (stderr, "A contract is missing from the plugin: %s\n", p.plugin->name);
		if (errOccurred)
		{
			*errOccurred = true;
		}

		keyDel (keyInfo);
		elektraPluginClose (p.plugin, NULL);
		p.plugin = NULL;

		if (freePsStrings)
		{
			elektraFree (p.ps.name);
			elektraFree (p.ps.refname);
			p.ps.name = NULL;
			p.ps.refname = NULL;

		}

		return p;
	}

	keyAddBaseName (keyInfo, "exports");

	elektraCursor it = ksSearch (p.ksInfo, keyInfo) + 1;
	if (it > 0)
	{
		struct PluginSymbols * curSymbol = p.symbols;
		for (; it < ksGetSize (p.ksInfo); it++)
		{
			k = ksAtCursor (p.ksInfo, it);
			if (keyIsBelow (keyInfo, k))
			{
				/* skip first part of keyname */
				curSymbol = addFuncToSymbolList (curSymbol, keyName (k) + keyGetNameSize (keyInfo), getFuncFromKey (k));
				if (!p.symbols)
				{
					p.symbols = curSymbol;
				}
			}
			else
			{
				break;
			}
		}
	}

	keySetBaseName (keyInfo, "infos");
	it = ksSearch (p.ksInfo, keyInfo) + 1;
	if (it > 0)
	{
		struct PluginInfos *curInfo = p.infos;
		for (; it < ksGetSize (p.ksInfo); it++)
		{
			k = ksAtCursor (p.ksInfo, it);
			if (keyIsBelow (keyInfo, k))
			{
				curInfo = addInfoToInfoList (curInfo, keyName (k) + keyGetNameSize (keyInfo), keyString (k));
				if (!p.infos)
				{
					p.infos = curInfo;
				}
			}
			else
			{
				break;
			}
		}
	}
	else
	{
		/* TODO: Handle error */
		fprintf (stderr, "No plugin info for plugin: %s\n", p.plugin->name);
		if (errOccurred)
		{
			*errOccurred = true;
		}

		keyDel (keyInfo);
		elektraPluginClose (p.plugin, NULL);
		p.plugin = NULL;

		while (p.symbols)
		{
			struct PluginSymbols * curNode = p.symbols;
			p.symbols = p.symbols->next;
			curNode->next = NULL;
			elektraFree (curNode);
		}

		if (freePsStrings)
		{
			elektraFree (p.ps.name);
			elektraFree (p.ps.refname);
			p.ps.name = NULL;
			p.ps.refname = NULL;
		}

		return p;
	}

	if (errOccurred)
	{
		*errOccurred = false;
	}

	return p;
}




/** @brief Parse a string containing information to create a KeySet
 *  @param pluginArguments comma (,) to separate key=value, contains no whitespaces
 *  @return newly created keyset with the information found in the string, make sure to ksDel() the returned KeySet.
 *  @return NULL if no '=' was found in pluginArguments
 */
// C++: KeySet parsePluginArguments (std::sstring const & pluginArguments, std::string const & basepath) from backendparser.cpp[35-54]
KeySet * cParsePluginArguments (char * pluginArguments, const char * basePath)
{
	if (!pluginArguments)
	{
		return NULL;
	}
	else if (!basePath)
	{
		basePath = "";
	}

	KeySet * ksRet = NULL;

	/* Read until the next '=', this should be the key name */
	bool equalFound = true;
	for (size_t posEqual, posComma; equalFound && (posEqual = strcspn (pluginArguments, "=")) > 0; pluginArguments +=  posEqual + posComma + 2)
	{
		/* Replace '=' with '\0' to use the current substring as parameter for a keyname */
		/* TODO: Handle error ('=' not found) */
		bool commaFound = false;
		if (pluginArguments[posEqual] == '=')
		{
			pluginArguments[posEqual] = '\0';
			posComma = strcspn (pluginArguments + posEqual + 1, ",");

			if (pluginArguments[posEqual + 1 + posComma] == ',')
			{
				commaFound = true;
				pluginArguments[posEqual + 1 + posComma] = '\0';
			}
		}
		else
		{
			equalFound = false;
		}

		/* elektraStrLen includes '\0', so we counted it two times, but use one byte for the '/' character */
		char * pluginKeyName = elektraMalloc (elektraStrLen (pluginArguments) + elektraStrLen (basePath));
		strcpy (pluginKeyName, basePath);
		strcat (pluginKeyName, "/");
		strcat (pluginKeyName, pluginArguments);

		Key * const keyToAppend = keyNew (pluginKeyName, KEY_VALUE, (equalFound ? pluginArguments + posEqual + 1 : NULL), KEY_END);

		/* The string gets copied by keyNew(), so we have to free the memory here. */
		elektraFree (pluginKeyName);

		if (!ksRet)
		{
			ksRet = ksNew (0, KS_END);
		}
		ksAppendKey (ksRet, keyToAppend);


		if (equalFound)
		{
			/* restore original values */
			pluginArguments[posEqual] = '=';
			if (commaFound)
			{
				pluginArguments[posEqual + 1 + posComma] = ',';
			}
		}
		else
		{
			break;
		}
	}

	return ksRet;
}


/**
 * @brief Process a single argument and add it to PluginSpecVector
 *
 * @internal
 *
 * @param [in,out] arguments current KeySet of processed arguments
 * @param [in,out] counter current counter, to be modified when argument is added
 * @param argument the argument to parse and add
 * @returns last node of the list (maybe NULL if psList was NULL, maybe newly allocated node, maybe existing last node)
 */
struct PluginSpecNode * processArgument (struct PluginSpecNode * psList, size_t * counter, char * argument)
{
	/* ignore empty or useless arguments (whitespace, ',' only) */
	if (!argument || !(*argument))
	{
		return psList;
	}

	const char * c = argument;
	for (; *c; c++)
	{
		if (!isspace (*c) && *c != ',')
		{
			break;
		}
	}

	if (!(*c))
	{
		/* only spaces and/or commas in string */
		return psList;
	}


	/* forward to last node */
	for (; psList && psList->next; psList = psList->next);


	/* check if argument contains '=' */
	if (strchr (argument, '='))
	{
		/* We have a configuration for a plugin */
		if (!psList)
		{
			/* TODO: Handle error */
			fprintf (stderr, "config for plugin (%s) without previous plugin name\n", argument);
			return NULL;
		}
		else
		{
			// C++: arguments.back ().appendConfig (parsePluginArguments (argument));

			KeySet * ksPluginArguments = cParsePluginArguments (argument, "user:");


			if (ksPluginArguments)
			{
				if (!(psList->ps.config))
				{
					psList->ps.config = ksNew (0, KS_END);
				}
				ksAppend (psList->ps.config, ksPluginArguments);
				ksDel (ksPluginArguments);
			}
		}
	}
	else
	{
		/* We have a plugin */
		/* Note that both, ps.name and ps.refname of the created node must be freed if they are not NULL */

		/* Create new node */
		if (!psList)
		{
			psList = elektraMalloc (sizeof (struct PluginSpecNode));
		}
		else
		{
			psList->next = elektraMalloc (sizeof (struct PluginSpecNode));
			psList = psList->next;
		}

		char * dupArgument = elektraStrDup (argument);
		if (setPluginFullName (&(psList->ps), dupArgument))
		{
			psList->ps.refname = NULL;

			/* TODO: Why use refnumber instead of refname? (taken from C++ code) */
			char * found = strchr (argument, '#');
			if (found)
			{
				/* cut off refname from string */
				*found = '\0';
				char * strRefNumber = elektraMalloc (10);
				if (snprintf (strRefNumber, 10, "%ld", (*counter)++) >= 10)
				{
					/* TODO: Handle overflow error */
					fprintf (stderr, "Overflow occured while converting size_t to string!\n");
					elektraFree (strRefNumber);
					return NULL;
				}
				else
				{
					psList->ps.refname = strRefNumber;
				}
			}
		}
		else
		{
			elektraFree (psList);
			return NULL;
		}
	}

	return psList;
}

/**
 * @brief Checks if reference name contains only numbers
 *
 * @return true if only numbers, false otherwise
 */
bool isRefNumber (struct PluginSpec ps)
{
	if (!ps.refname) return false;

	for (const char * c = ps.refname; *c; c++)
	{
		if (*c < '0' || *c > '9') return false;
	}

	return true;
}





/* returned string is part of a key in the KeySet `info` */
const char * pluginLookupInfo (const struct Plugin * p, const char * item, const char * section)
{
	if (!section || !(*section))
	{
		section = "infos";
	}

	/* TODO: store key-name in constant */
	Key * k = keyNew ("system:/elektra/modules", KEY_END);
	keyAddBaseName (k, p->ps.name);
	keyAddBaseName (k, section);
	keyAddBaseName (k, item);
	Key * ret = ksLookup (p->ksInfo, k, KDB_O_NONE);
	keyDel (k);

	if (ret)
	{
		return keyString (ret);
	}
	else
	{
		/* TODO: Let's say missing info is ok for now */
		return NULL;
	}
}

/* the returned string must be freed */
char * modulesPluginDatabaseLookupInfo (struct PluginSpec ps, const char * info, KeySet * ksModules)
{
	ksAppendKey (ps.config, keyNew ("system:/module", KEY_VALUE, "this plugin was loaded for the status", KEY_END));
	// C++: PluginPtr plugin = impl->modules.load (spec.getName (), conf);

	bool errOccurred;
	char * result = NULL;
	struct Plugin p = loadPluginFromSpec (ps, ksModules, &errOccurred);

	if (!errOccurred)
	{
		result = strdup (pluginLookupInfo (&p, info, "infos"));
	}

	/* Cleanup */
	freePlugin (true, p, ps);

	return result;
}


// C++: PluginSpec ModulesPluginDatabase::lookupMetadata (std::string const & which) const from plugindatabase.cpp[253]
struct PluginSpec modulesPluginDatabaseLookupMetadata (const char * metaName, KeySet * ksModules)
{
	struct PluginSpec retErr = {NULL, NULL, NULL};
	if (!metaName || !(*metaName))
	{
		return retErr;
	}

	bool freeStrings;
	char **allPluginNames = getAllPluginNames (&freeStrings);
	struct PluginSpecScoreNode * foundPlugins = NULL;
	struct PluginSpecScoreNode * curFoundPlugin = NULL;

	struct PluginSpec ps;
	ps.config = ksNew (5, keyNew ("system:/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END);

	/* Collect possible plugins */
	for (char ** curPluginName = allPluginNames; curPluginName; curPluginName++)
	{
		/* TODO: remove /module hack (taken from C++ code) */
		if (!setPluginFullName (&ps, *curPluginName))
		{
			/* TODO: handle error */
			ksDel (ps.config);
			freeStrArr (allPluginNames, freeStrings);
			return retErr;
		}

		char * const infoMetadata = modulesPluginDatabaseLookupInfo (ps, "metadata", ksModules);
		struct StringNode * metadataList = splitAndMoveStringToStringList (NULL, infoMetadata);

		for (struct StringNode * curMetadata = metadataList; curMetadata; curMetadata = curMetadata->next)
		{
			if (curMetadata->str && (elektraStrCmp (metaName, curMetadata->str) == 0))
			{
				char * const infoStatus = modulesPluginDatabaseLookupInfo (ps, "status", ksModules);
				if (infoStatus && *infoStatus)
				{
					struct PluginSpec newSpec;
					newSpec.config = NULL;
					setPluginFullName (&newSpec, *curPluginName);

					curFoundPlugin = appendPluginScore (curFoundPlugin, newSpec, calculatePluginScore(infoStatus));

					if (!foundPlugins)
					{
						foundPlugins = curFoundPlugin;
					}
				}

				elektraFree (infoStatus);
				break;
			}
		}

		freeStringList (metadataList);
		elektraFree (infoMetadata);
	}

	ksDel (ps.config);

	if (!foundPlugins)
	{
		fprintf (stderr, "No plugin that provides metadata %s could be found.\n", metaName);
		freeStrArr (allPluginNames, freeStrings);
		return retErr;
	}
	else
	{
		/* Determine plugin with highest rank */
		int maxPoints = INT_MIN;
		struct PluginSpec bestPs = retErr;

		for (curFoundPlugin = foundPlugins; curFoundPlugin; curFoundPlugin = curFoundPlugin->next)
		{
			if (curFoundPlugin->points > maxPoints)
			{
				maxPoints = curFoundPlugin->points;
				bestPs = curFoundPlugin->ps;
			}
		}
		freePluginSpecScoreList (foundPlugins, false);

		/* Dup string with plugin-name (current name is taken from str-array, which gets freed before this function returns */
		setPluginFullName (&bestPs, getPluginFullName (bestPs));

		freeStrArr (allPluginNames, freeStrings);
		return bestPs;
	}
}



// C++: std::map<int, PluginSpec> ModulesPluginDatabase::lookupAllProvidesWithStatus (std::string const & which) const
struct PluginSpecScoreNode * lookupAllProvidesWithStatus (const char * const pluginName, KeySet * ksModules)
{
	bool freeStrings;
	char ** allPluginNames = getAllPluginNames (&freeStrings);

	struct PluginSpecScoreNode * foundPlugins = NULL;
	struct PluginSpecScoreNode * curFoundPlugin = NULL;

	struct PluginSpec ps;
	ps.config = ksNew (5, keyNew ("system:/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END);

	for (char ** curPluginName = allPluginNames; curPluginName; curPluginName++)
	{
		/* TODO: make sure (non)-equal plugins (i.e. with same/different contract) are handled correctly */

		if (!setPluginFullName (&ps, *curPluginName))
		{
			/* TODO: handle error */
			ksDel (ps.config);
			freeStrArr (allPluginNames, freeStrings);
			return NULL;
		}

		/* Let's see if there is a plugin named after the required provider */
		if (elektraStrCmp (*curPluginName, pluginName) == 0)
		{
			// C++: int s = calculateStatus (lookupInfo (spec, "status"));
			char * const luInfo = modulesPluginDatabaseLookupInfo (ps, "status", ksModules);

			if (luInfo && *luInfo)
			{
				struct PluginSpec newSpec;
				newSpec.config = NULL;
				/* strdup needed because the string is contained in the string array
				 * which gets freed at the end of this function */
				setPluginFullName(&newSpec, elektraStrDup(*curPluginName));

				curFoundPlugin = appendPluginScore (curFoundPlugin, newSpec, calculatePluginScore(luInfo));

				if (!foundPlugins)
				{
					foundPlugins = curFoundPlugin;
				}
				/* We are done with the current plugin */
				continue;
			}

			elektraFree (luInfo);
		}

		/* TODO: Support for generic plugins with config */
		char * const luInfoProvides = modulesPluginDatabaseLookupInfo (ps, "provides", ksModules);

		for (char *it = luInfoProvides, *strPrevStart = luInfoProvides; it; ++it)
		{
			if (!(*it) || isspace (*it))
			{
				bool lastIteration = (*it) ? false : true;
				*it = '\0';

				if (elektraStrCmp (strPrevStart, pluginName) == 0)
				{

					struct PluginSpec newSpec;
					newSpec.config = NULL;
					setPluginFullName (&newSpec, elektraStrDup(*curPluginName));

					char * const luInfoStatus = modulesPluginDatabaseLookupInfo (ps, "status", ksModules);
					curFoundPlugin = appendPluginScore (curFoundPlugin, newSpec, calculatePluginScore(luInfoStatus));
					elektraFree (luInfoStatus);

					if (!foundPlugins)
					{
						foundPlugins = curFoundPlugin;
					}
				}

				if (lastIteration)
				{
					break;
				}
				else
				{
					strPrevStart = it + 1;
				}
			}
		}
		elektraFree (luInfoProvides);
	}

	freeStrArr (allPluginNames, freeStrings);
	ksDel (ps.config);

	if (!foundPlugins)
	{
		/* TODO: Error handling */
		fprintf(stderr, "No plugin that provides %s could be found!\n", pluginName);
	}

	return foundPlugins;
}


/* from plugindatabase.cpp[104-139] */
bool hasProvides (const char * infoProvides, KeySet * ksModules)
{
	bool freeStrings;
	char ** allPlugins = getAllPluginNames (&freeStrings);

	for (char ** curPlugin = allPlugins; curPlugin; ++curPlugin)
	{
		// C++: pd.lookupInfo (...)
		struct PluginSpec ps;
		setPluginFullName (&ps, *curPlugin);
		KeySet * ksPluginConfig =
			ksNew (5, keyNew ("system:/module", KEY_VALUE, "this plugin was loaded without a config", KEY_END), KS_END);
		ps.config = ksPluginConfig;

		char * const strLookupInfo = modulesPluginDatabaseLookupInfo (ps, "provides", ksModules);

		char * prevStart = strLookupInfo;
		for (char * it = strLookupInfo; it && *it; ++it)
		{
			if (isspace (*it))
			{
				*it = '\0';
				if (elektraStrCmp (prevStart, infoProvides))
				{
					ksDel (ksPluginConfig);
					elektraFree (strLookupInfo);
					freeStrArr (allPlugins, freeStrings);
					return true;
				}
				prevStart = it + 1;
			}
		}
		ksDel (ksPluginConfig);
		elektraFree (strLookupInfo);
	}

	freeStrArr (allPlugins, freeStrings);
	return false;
}



// C++: PluginDatabase::Status ModulesPluginDatabase::status (PluginSpec const & spec) const in plugindatabase.cpp[209-230]
enum PluginStatus getPluginStatus (struct PluginSpec spec, KeySet * ksModules)
{
	ksAppendKey (spec.config, keyNew ("system:/module", KEY_VALUE, "this plugin was loaded for the status", KEY_END));

	bool errOccurred;
	struct Plugin p = loadPluginFromSpec (spec, ksModules, &errOccurred);
	freePlugin (true, p, spec);

	if (errOccurred)
	{
		if (hasProvides (spec.name, ksModules))
		{
			return provides;
		}
		else
		{
			return missing;
		}
	}
	else
	{
		return real;
	}
}

// C++: PluginSpec ModulesPluginDatabase::lookupProvides (std::string const & which) const in plugindatabase.cpp[306]
struct PluginSpec lookupProvides (const char * const pluginName, KeySet * ksModules)
{
	struct PluginSpec retErr = {NULL, NULL, NULL};
	if (!validatePluginName(pluginName))
	{
		/* TODO: Handle error */
		return retErr;
	}


	/* Check if plugin with provider name exists */
	struct PluginSpec ps;
	setPluginFullName (&ps, strdup(pluginName));
	ps.config = NULL;

	if (getPluginStatus (ps, ksModules) == real)
	{
		return ps;
	}
	else
	{
		elektraFree (ps.name);
	}

	/* C++: lookupAllProvidesWithStatus (pluginName) */
	struct PluginSpecScoreNode * foundPlugins  = lookupAllProvidesWithStatus (pluginName, ksModules);
	if (!foundPlugins)
	{
		/* TODO: error handling */
		fprintf (stderr, "No plugins found!\n");
		return retErr;
	}
	else
	{

		/* Determine the plugin with the highest rank */
		int maxPoints = INT_MIN;
		struct PluginSpec bestPs = retErr;

		for (struct PluginSpecScoreNode * curFoundPlugin = foundPlugins; curFoundPlugin; curFoundPlugin = curFoundPlugin->next)
		{
			if (curFoundPlugin->points > maxPoints)
			{
				maxPoints = curFoundPlugin->points;
				bestPs = curFoundPlugin->ps;
			}
		}

		/* Free other pluginSpecs */
		for (struct PluginSpecScoreNode * curFoundPlugin = foundPlugins, *nextPlugin; curFoundPlugin; curFoundPlugin = nextPlugin)
		{
			nextPlugin = curFoundPlugin->next;

			if ( !(curFoundPlugin->points == maxPoints && curFoundPlugin->ps.name == bestPs.name && curFoundPlugin->ps.refname == bestPs.refname))
			{
				/* lookupAllProvidesWithStatus sets only ps.name, but not ps.refname or ps.config */
				elektraFree (curFoundPlugin->ps.name);
				elektraFree (curFoundPlugin);
			}
		}

		return bestPs;
	}
}


/* Check if dependency is relevant (occurs in KeySet) */
void checkDependencyRelevancy (const KeySet * deps, const char * order)
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
				Key * k = ksAtCursor (deps, j);
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
 * @returns sorted list
 */
// C++: void BackendBuilder::sort () in backendbuilder.cpp[93-177]
struct PluginSpecNode * sortPluginSpecList (struct PluginSpecNode * pluginSpecsToSort, KeySet * ksModules)
{
	KeySet * deps = ksNew (0, KS_END);
	size_t i = 0;

	for (struct PluginSpecNode * curNode = pluginSpecsToSort; curNode; curNode = curNode->next)
	{
		char * depkeyName = elektraMalloc (elektraStrLen (curNode->ps.name) + 1);
		*depkeyName = '/';
		strcpy(depkeyName + 1, curNode->ps.name);

		Key * dep = keyNew (depkeyName, KEY_END);
		elektraFree (depkeyName);

		if (elektraStrCmp (curNode->ps.name, curNode->ps.refname) != 0)
		{
			keyAddBaseName (dep, curNode->ps.refname);
		}

		ksAppendKey (deps, dep);
		char depStr[10];
		if (snprintf (depStr, 10, "%zu", i) >= 10)
		{
			/* TODO: Handle overflow error */
			ksDel (deps);
			return NULL;
		}

		keySetString (dep, depStr);
		keySetMeta (dep, "order", depStr);
		i++;
	}


	/* points to first node of list */
	struct StringNode * addedDeps = NULL;

	/* points to last node of list */
	struct StringNode * lastAddedDep = NULL;

	for (struct PluginSpecNode * curNode = pluginSpecsToSort; curNode; curNode = curNode->next)
	{
		char * const luInfo = modulesPluginDatabaseLookupInfo(curNode->ps, "ordering", ksModules);

		/* Copy is needed because nodes with substrings can be deleted --> give every substring its own allocated memory */
		struct StringNode * orderList = splitAndCopyStringToStringList(NULL, luInfo);
		elektraFree (luInfo);

		while (orderList)
		{
			bool alreadyInserted = false;
			for (struct StringNode * curAddedDep = addedDeps; !alreadyInserted && curAddedDep; curAddedDep = curAddedDep->next)
			{
				if (elektraStrCmp(curAddedDep->str, orderList->str) == 0)
				{
					alreadyInserted = true;
				}
			}

			/* remove (or move to addedDeps) first node from order list */
			if (alreadyInserted)
			{
				struct StringNode * tmpNode = orderList;
				orderList = orderList->next;
				tmpNode->next = NULL;
				freeStringList (tmpNode);
			}
			else
			{
				if (!addedDeps)
				{
					addedDeps = lastAddedDep = orderList;
				}
				else
				{
					lastAddedDep->next = orderList;

					/* new last node */
					lastAddedDep = lastAddedDep->next;
				}

				lastAddedDep->next = NULL;
				orderList = orderList->next;

				/* Check if dependency is relevant (occurs in KeySet) */
				/* This call may adds metadata to keys in the KeySet 'deps' */
				checkDependencyRelevancy (deps, lastAddedDep->str);
			}
		}
	}


	/* Now sort by the given topology */
	Key ** keyArray = elektraMalloc (ksGetSize (deps) * sizeof (Key *));
	int ret = elektraSortTopology (deps, keyArray);
	if (ret == 0)
	{
		/* TODO: Handle error */
		ksDel (deps);
		elektraFree (keyArray);
		fprintf (stderr, "CyclicOrderViolation\n");
		return NULL;
	}
	else if (ret == -1)
	{
		/* TODO: Handle error */
		ksDel (deps);
		elektraFree (keyArray);
		fprintf (stderr, "elektraSortTopology was used wrongly");
		return NULL;
	}


	struct PluginSpecNode * sortedListStartNode = NULL;
	struct PluginSpecNode * sortedListEndNode = NULL;

	long * usedIndices = elektraMalloc(ksGetSize(deps) * sizeof(long));
	long numUsedIndices = 0;

	for (elektraCursor it = 0; it < ksGetSize(deps); it++)
	{
		Key * curKey = *(keyArray + it);
		if (!curKey)
		{
			/* TODO: Handle error */
			fprintf (stderr, "Key from keyArray was NULL!\n");
			elektraFree (usedIndices);
			elektraFree (keyArray);
			return NULL;
		}

		char * eptr;
		long curIndex = strtol (keyString (curKey), &eptr, 10);

		if (*eptr != '\0')
		{
			/* TODO: Handle error */
			fprintf (stderr, "Could not convert string to long!\n");
			elektraFree (usedIndices);
			elektraFree (keyArray);
			return NULL;
		}

		struct PluginSpecNode * curNode = pluginSpecsToSort;
		struct PluginSpecNode * prevNode = NULL;
		for (long l = 0; l < curIndex; l++)
		{
			bool indexUsed = false;
			for (long usedIndexIt = 0; usedIndexIt < numUsedIndices; usedIndexIt++)
			{
				if (usedIndices[usedIndexIt] == l)
				{
					indexUsed = true;
					break;
				}
			}

			if (indexUsed)
			{
				/* node with that index was already moved to result list */
				continue;
			}
			else if (curNode->next == NULL)
			{
				/* TODO: Check if that should be treated as error */
				fprintf (stderr, "Node for sorting was NULL!\n");
				elektraFree (usedIndices);
				elektraFree (keyArray);
				return NULL;
			}
			else
			{
				prevNode = curNode;
				curNode = curNode->next;
			}
		}

		usedIndices[numUsedIndices++] = curIndex;

		/* move node to end of sorted result list */
		if (!sortedListStartNode)
		{
			sortedListStartNode = sortedListEndNode = curNode;
		}
		else
		{
			sortedListEndNode->next = curNode;
			sortedListEndNode = sortedListEndNode->next;
			sortedListEndNode->next = NULL;
		}

		/* curNode was moved to result list */
		prevNode->next = prevNode->next->next;
	}

	elektraFree (usedIndices);
	elektraFree (keyArray);

	return sortedListStartNode;
}





// C++: backendbuilder.cpp[424-489]
struct PluginSpec backendBuilderAddPlugin (struct PluginSpecNode * existingSpecs, struct PluginSpec ps, KeySet ** ksBackendConf, KeySet * ksModules)
{
	char * pluginFullName = getPluginFullName (ps);

	if (!pluginFullName)
	{
		return ps;
	}

	for (const struct PluginSpecNode * curSpec = existingSpecs; curSpec; curSpec = curSpec->next)
	{
		char * curFullName = getPluginFullName (curSpec->ps);
		if (elektraStrCmp (curFullName, pluginFullName) == 0)
		{
			/* TODO: Handle error */
			/* in C++: throw PluginAlreadyInserted (plugin.getFullName ()); */
			fprintf (stderr, "Plugin already inserted!\n");
			elektraFree (curFullName);
			elektraFree (pluginFullName);
			return ps;
		}
		elektraFree (curFullName);
	}
	elektraFree (pluginFullName);



	/* TODO: Refactor - put check for already inserted plugins in own function */

	/* If the plugin is actually a provider use it (otherwise we will get our name back) */
	/* The returned struct 'provides' has only ps.name set, this string must be freed! */
	struct PluginSpec provides = lookupProvides (ps.name, ksModules);


	//if (!provides.name)
	//{
	//	/* TODO: Handle error */
	//	fprintf (stderr, "Lookup provides returned an error!\n");
	//}

	if (provides.name && elektraStrCmp (provides.name, ps.name) != 0)
	{
		/* Keep our config and refname */
		elektraFree (ps.name);
		ps.name = provides.name;

		if (ksGetSize(provides.config) > 0)
		{
			if (!ps.config)
			{
				ps.config = ksNew (0, KS_END);
			}

			ksAppend (ps.config, provides.config);
			ksDel (provides.config);
		}
	}
	else
	{
		elektraFree (provides.name);
		ksDel (provides.config);
	}

	/* Call the checkconf-function of the plugin (if provided)
	 * this enables a plugin to verify its configuration at mount time */
	// C++: checkConfPtr checkConfFunction = reinterpret_cast<checkConfPtr> (pluginDatabase->getSymbol (newPlugin, "checkconf"));
	/* TODO: add error handling */
	struct Plugin newPlugin = loadPluginFromSpec (ps, ksModules, NULL);
	int (*checkConfFunction) (Key *, KeySet *) = (int (*) (Key *, KeySet *)) getSymbolFromList(newPlugin.symbols, "checkconf");

	if (checkConfFunction)
	{
		Key * errorKey = keyNew ("/", KEY_END);

		/* merge plugin config and backend config together */
		if (!ps.config)
		{
			ps.config = ksNew(0, KS_END);
		}

		KeySet * pluginConfig = ksDup (ps.config);
		ksAppend (pluginConfig, *ksBackendConf);

		/* Call the checkconf function of the plugin */
		int checkResult = checkConfFunction (errorKey, pluginConfig);
		if (checkResult == -1)
		{
			/* TODO: Handle error */
			ksDel (pluginConfig);
			fprintf (stderr, "PluginConfigInvalid: %s", keyString (errorKey));

			freePlugin (true, newPlugin, ps);
			return ps;
		}
		else if (checkResult == 1)
		{
			/* Separate plugin config from the backend config */
			Key * backendParent = keyNew ("system:/", KEY_END);

			/* Take over the new configuration */
			ksDel (*ksBackendConf);
			*ksBackendConf = ksCut (pluginConfig, backendParent);

			ksDel (ps.config);
			ps.config = pluginConfig;

			keyDel (backendParent);
		}
		else
		{
			/* TODO: Handle error */
			ksDel (pluginConfig);
		}
		keyDel (errorKey);
	}

	freePlugin(true, newPlugin, ps);

	return ps;
}

/* returns the sorted list with the new pluginSpec added (other nodes are taken from _existingSpecs_) */
// new function, was part of backendBuilderAddPlugin in C++
struct PluginSpecNode * backendBuilderAddAndInsertPlugin (struct PluginSpecNode * existingSpecs, struct PluginSpec ps, KeySet ** ksBackendConf, KeySet * ksModules)
{
	struct PluginSpec psNew = backendBuilderAddPlugin (existingSpecs, ps, ksBackendConf, ksModules);

	if (!psNew.name && !psNew.refname && !psNew.config)
	{
		/* TODO: Check if strings in ps should be freed */
		return NULL;
	}

	/* add new pluginSpec to end of list */
	if (!existingSpecs)
	{
		existingSpecs = elektraMalloc (sizeof (struct PluginSpecNode));
		existingSpecs->next = NULL;
		existingSpecs->ps = psNew;
	}
	else
	{
		struct PluginSpecNode * lastExistingSpec;
		for (lastExistingSpec = existingSpecs; lastExistingSpec->next; lastExistingSpec = lastExistingSpec->next);
		lastExistingSpec->next = elektraMalloc (sizeof (struct PluginSpecNode));
		lastExistingSpec = lastExistingSpec->next;
		lastExistingSpec->next = NULL;
		lastExistingSpec->ps = psNew;
	}

	return sortPluginSpecList(existingSpecs, ksModules);
}

/* C++: replaces void BackendBuilder::collectNeeds (std::vector<std::string> & needs) const and
 * void BackendBuilder::collectRecommends (std::vector<std::string> & recommends) const in backendbuilder.cpp[225,243] */
struct StringNode * collectInfo (struct StringNode * existingInfos, struct PluginSpecNode * existingPlugins, const char * info,
	KeySet * ksModules)
{
	if (!existingPlugins) return NULL;

	struct StringNode * curInfoNode = existingInfos;
	for (; existingPlugins; existingPlugins = existingPlugins->next)
	{
		char * const luInfo = modulesPluginDatabaseLookupInfo (existingPlugins->ps, info, ksModules);

		/* Create a copy so that every substring gets its own allocated memory and can be freed together with a node of the list */
		curInfoNode = splitAndCopyStringToStringList(curInfoNode, luInfo);
		elektraFree (luInfo);

		if (!existingInfos)
		{
			/* New list (start node) created */
			existingInfos = curInfoNode;
		}
	}

	return existingInfos;
}

// C++: void BackendBuilder::removeProvided (std::vector<std::string> & needs) const from backendbuilder.cpp[256-272]
struct StringNode * removeProvided (struct StringNode * toProcess, const struct PluginSpecNode * psList, KeySet * ksModules)
{
	for (; toProcess && psList; psList = psList->next)
	{
		/* Remove the needed plugins that are already inserted */
		// C++: needs.erase (std::remove (needs.begin (), needs.end (), ps.getName ()), needs.end ());
		toProcess = removeFromStringList (toProcess, psList->ps.name, true);

		if (!toProcess) break;

		/* Remove what is already provided */
		char * const luProvides = modulesPluginDatabaseLookupInfo (psList->ps, "provides", ksModules);
		struct StringNode * const listProvides = splitAndMoveStringToStringList (NULL, luProvides);

		for (struct StringNode * curProvidesNode = listProvides; toProcess && curProvidesNode; curProvidesNode = curProvidesNode->next)
		{
			toProcess = removeFromStringList (toProcess, curProvidesNode->str, true);
		}

		freeStringList (listProvides);
		elektraFree (luProvides);
	}

	return toProcess;
}

// C++: void removeMissing (std::vector<std::string> & recommendedPlugins, std::vector<std::string> const & missingPlugins) from backendbuilder.cpp[292-298]
struct StringNode * removeMissing (struct StringNode * recommendedPlugins, const struct StringNode * missingPlugins)
{
	for (; recommendedPlugins && missingPlugins; missingPlugins = missingPlugins->next)
	{
		recommendedPlugins = removeFromStringList(recommendedPlugins, missingPlugins->str, true);
	}

	return recommendedPlugins;
}

struct StringNode * removeMetadata (struct StringNode * metaDataList, const struct PluginSpecNode * psList, KeySet * ksModules)
{
	for (; metaDataList && psList; psList = psList->next)
	{
		/* Remove metadata that is already provided */
		char * const strMeta = modulesPluginDatabaseLookupInfo (psList->ps, "metadata", ksModules);
		struct StringNode * const listMeta = splitAndMoveStringToStringList (NULL, strMeta);

		for (const struct StringNode * curMeta = listMeta; metaDataList && curMeta; curMeta = curMeta->next)
		{
			metaDataList = removeFromStringList (metaDataList, curMeta->str, true);
		}

		freeStringList (listMeta);
		elektraFree (strMeta);
	}

	return metaDataList;
}


/**
 * @brief Fix refnames after parsing
 *
 * @internal
 *
 * @param arguments to fix
 */
void fixArguments (struct PluginSpecNode * psList)
{
	/* Fix refnames of single occurrences for backwards compatibility and cleaner names */
	for (struct PluginSpecNode * curNode = psList; psList; psList = psList->next)
	{
		/* Count number of PluginSpecs in psList with same name */
		size_t numCurPluginName = 0;
		for (struct PluginSpecNode * nodeCmp = psList; nodeCmp; nodeCmp = nodeCmp->next)
		{
			if (elektraStrCmp (curNode->ps.name, nodeCmp->ps.name) == 0)
			{
				numCurPluginName++;
			}
		}

		assert (numCurPluginName >= 1);

		// C++: if (nr == 1 && a.isRefNumber ())
		if (numCurPluginName == 1 && isRefNumber (curNode->ps))
		{
			elektraFree (curNode->ps.refname);

			/* Now only one of the two pointers must be freed, as they point to the same location! */
			curNode->ps.refname = curNode->ps.name;
		}

		// C++: size_t identical = std::count_if (arguments.begin (), arguments.end (), std::bind (PluginSpecRefName (), a, std::placeholders::_1));
		size_t numIdentical = 0;
		for (struct PluginSpecNode * nodeCmp = psList; nodeCmp; nodeCmp = nodeCmp->next)
		{
			if (elektraStrCmp (curNode->ps.refname, nodeCmp->ps.refname) == 0)
			{
				numIdentical++;
			}

			assert (numIdentical >= 1);

			if (numIdentical > 1)
			{
				/* TODO: Handle error */
				char * curPluginFullName = getPluginFullName (curNode->ps);
				fprintf (stderr, "Identical reference names found for plugin: %s\n", curPluginFullName);
				elektraFree (curPluginFullName);
			}
		}
	}

	/* Now fix counter to be minimal */
	size_t counter = 0;
	for (struct PluginSpecNode * curNode = psList; psList; psList = psList->next)
	{
		if (isRefNumber (curNode->ps))
		{
			char * strCounter = elektraMalloc (10);

			if (snprintf (strCounter, 10, "%ld", counter++) >= 10)
			{
				/* TODO: Handle overflow error */
				elektraFree (strCounter);
				fprintf (stderr, "Overflow error while converting long to string!\n");
			}
			else
			{
				if (curNode->ps.name != curNode->ps.refname)
				{
					elektraFree (curNode->ps.refname);
				}
				curNode->ps.refname = strCounter;
			}
		}
	}
}


/**
 * @brief Parse a plugins-string
 *
 * @param plugins contains space separated plugins with optional plugin configurations
 *
 * @note currently whitespaces are not allowed within pluginname or config, use
 * iterator interface parseArguments() if you need it.
 *
 * @see parseArguments()
 * @return A KeySet with parsed PluginSpecs
 */

struct PluginSpecNode * parseArguments (char * strPlugins)
{
	if (!strPlugins)
	{
		return NULL;
	}

	/* C++ return parseArguments (args.begin (), args.end ()); */
	struct PluginSpecNode * resultList = NULL;
	struct PluginSpecNode * curNode = NULL;

	size_t counter = 0;
	for (bool strEndReached = (*strPlugins) ? false : true; !strEndReached; strPlugins++)
	{
		if (*strPlugins == ' ' || !(*strPlugins))
		{
			if (*strPlugins == ' ')
			{
				*strPlugins = '\0';
			}
			else
			{
				strEndReached = true;
			}

			/* The function processArgument creates a copy of strPlugins, if needed */
			curNode = processArgument (curNode, &counter, strPlugins);

			if (!resultList)
			{
				resultList = curNode;
			}

			if (!strEndReached)
			{
				/* restore space char */
				*strPlugins = ' ';
			}
		}
	}

	fixArguments (resultList);
	return resultList;
}


/**
 * @brief resolve all needs that were not resolved by adding plugins.
 *
 * @warning Must only be used once after all plugins/recommends are added,
 * memory pointed to by parameters @p neededPlugins, @p recommendedPlugins and @p metaData is invalid after this call
 *
 * @return the missing recommended plugins
 * @retval NULL if addRecommends was false
 *
 * @see addPlugin()
 */
// C++: std::vector<std::string> BackendBuilder::resolveNeeds (bool addRecommends) from backendbuilder.cpp[334]
struct StringNode * resolveNeeds (bool addRecommends, struct PluginSpecNode * psList, struct StringNode * neededPlugins,
	struct StringNode * recommendedPlugins, struct StringNode * metaData, KeySet ** backendConf, KeySet * ksModules)
{
	/* Load dependency-plugins immediately */
	for (; psList; psList = psList->next)
	{
		char * const curInfo = modulesPluginDatabaseLookupInfo (psList->ps, "plugins", ksModules);
		struct PluginSpecNode * psListParsed = parseArguments (curInfo);

		/* parseArguments copies to string curInfo, if needed, so it should be freed now */
		elektraFree (curInfo);

		for (struct PluginSpecNode * psParsedCur = psListParsed; psParsedCur; psParsedCur = psParsedCur->next)
		{
			/* returns sorted list */
			psList = backendBuilderAddAndInsertPlugin (psList, psParsedCur->ps, backendConf, ksModules);
		}
	}

	struct StringNode * missingRecommends = NULL;

	do
	{
		// C++: collectNeeds (neededPlugins);
		neededPlugins = collectInfo (neededPlugins, psList, "needs", ksModules);

		// C++: collectRecommends (recommendedPlugins);
		recommendedPlugins = collectInfo (recommendedPlugins, psList, "recommends", ksModules);

		// C++: removeProvided (neededPlugins);
		neededPlugins = removeProvided (neededPlugins, psList, ksModules);

		// C++: removeProvided (recommendedPlugins);
		recommendedPlugins = removeProvided (recommendedPlugins, psList, ksModules);

		// C++: removeMissing (recommendedPlugins, missingRecommends);
		recommendedPlugins = removeMissing (recommendedPlugins, missingRecommends);

		// C++: removeMetadata (metadata);
		// TODO: Check if metaData should be sorted (was set of strings in C++)
		metaData = removeMetadata (metaData, psList, ksModules);

		/* Leftover in needs (metadata) is what is still needed --> let's add first one */
		if (neededPlugins)
		{
			struct PluginSpec ps;
			char * strNeededPlugin;

			if (neededPlugins->shouldFreeStr)
			{
				/* Take ownership of allocated memory for the string */
				strNeededPlugin = neededPlugins->str;
				neededPlugins->shouldFreeStr = false;
			}
			else
			{
				/* Dup string so that the memory can be freed */
				strNeededPlugin = elektraStrDup (neededPlugins->str);
			}

			setPluginFullName (&ps, strNeededPlugin);
			psList = backendBuilderAddAndInsertPlugin (psList, ps, backendConf, ksModules);

			/* remove first node from list */
			struct StringNode * tmpNode = neededPlugins;
			neededPlugins = neededPlugins->next;
			tmpNode->next = NULL;
			freeStringList (tmpNode);
		}
		else if (metaData)
		{
			struct PluginSpec metaPs = modulesPluginDatabaseLookupMetadata (metaData->str, ksModules);
			psList = backendBuilderAddAndInsertPlugin (psList, metaPs, backendConf, ksModules);
			elektraFree (metaPs.name);

			/* remove first node from list */
			struct StringNode * tmpNode = metaData;
			metaData = metaData->next;
			tmpNode->next = NULL;
			freeStringList (tmpNode);
		}
		else if (recommendedPlugins && addRecommends)
		{
			struct PluginSpec ps;
			char * strRecommendedPlugin;

			/* String gets added to psList or missingRecommends --> don't delete it here! */
			if (recommendedPlugins->shouldFreeStr)
			{
				/* Take ownership of allocated memory for the string */
				strRecommendedPlugin = recommendedPlugins->str;
				recommendedPlugins->shouldFreeStr = false;
			}
			else
			{
				/* Dup string so that the memory can be freed */
				strRecommendedPlugin = elektraStrDup (recommendedPlugins->str);
			}

			setPluginFullName (&ps, strRecommendedPlugin);

			if (getPluginStatus (ps, ksModules) == missing)
			{
				psList = backendBuilderAddAndInsertPlugin (psList, ps, backendConf, ksModules);
			}
			else
			{
				if (missingRecommends)
				{
					addStrAtEnd (missingRecommends, recommendedPlugins->str, true);
				}
				else
				{
					missingRecommends = addStrAtEnd (NULL, recommendedPlugins->str, true);
				}
			}

			/* remove first node from list */
			struct StringNode * tmpNode = recommendedPlugins;
			recommendedPlugins = recommendedPlugins->next;
			tmpNode->next = NULL;
			freeStringList (tmpNode);
		}
	} while (neededPlugins || metaData || (recommendedPlugins && addRecommends));


	if (!addRecommends)
	{
		freeStringList (recommendedPlugins);
	}

	return missingRecommends;
}



bool pluginsCheckInternal (char * info, const struct StringNode * listToCheck)
{
	for (char * c = (char *) info; c; c++)
	{
		if (isspace (*c) || *c == '\0')
		{
			/* process current word in the string */
			/* Be aware that the string in the plugin gets changed temporarily */
			char savedChar = *c;
			*c = '\0';

			/* Simply look in the already provided names,
		 	 * because both, plugin names + provided names,
		 	 * are there.
		 	 * If it is found, we have an ordering violation. */
			for (; listToCheck; listToCheck = listToCheck->next)
			{

				if (elektraStrCmp (listToCheck->str, info) == 0)
				{
					/* TODO: correct logging (instead of fprintf) */
					// C++: throw OrderingViolation ();
					*c = savedChar;
					return false; /* violation detected --> caller should handel error */
				}
			}

			if (savedChar != '\0')
			{
				*c = savedChar;

				/* start of next word in the string */
				info = c + 1;
			}
			else
			{
				break;
			}
		}
	}

	return true; /* no violation detected */
}


// C++: from plugins.cpp[191-226]
/* Check conflicts of plugins */
bool pluginsCheckConflicts (const struct Plugin * p, const struct StringNode * listAlreadyProvided, const struct StringNode * listAlreadyConflict)
{
	/* TODO: replace NULL with ksInfo (decided where to get it (in the function or as argument) */
	const char * luInfo = pluginLookupInfo (p, "conflicts", "infos");

	/* Check already provided plugins */
	if (!pluginsCheckInternal ((char *) luInfo, listAlreadyProvided))
	{
		// C++: throw ConflictViolation ();
		return false;
	}

	/* Is there a conflict against the name? */
	if (!pluginsCheckInternal (p->ps.name, listAlreadyConflict))
	{
		// C++: throw ConflictViolation ();
		return false;
	}

	/* Is there a conflict against what it provides? */
	luInfo = pluginLookupInfo (p, "provides", "infos");
	return pluginsCheckInternal ((char *) luInfo, listAlreadyConflict);
}




// C++: from plugins.cpp[171-187]
/* Check ordering of plugins */
bool pluginsCheckOrdering (const struct Plugin * p, const struct StringNode * listAlreadyProvided)
{
	const char * luInfo = pluginLookupInfo (p, "ordering", "infos");
	/* The content of luInfo only gets changed temporarily, after the function returns,
	 * the original content is restored, therefore we cast the const away here. */
	return pluginsCheckInternal ((char *) luInfo, listAlreadyProvided);
}


// C++: plugin.cpp[370-382]
bool pluginFindInfo (const struct Plugin * plugin, const char * compare, const char * item, const char * section)
{
	if (!section || !(*section)) section = "infos";

	const char * luInfo = pluginLookupInfo (plugin, item, section);

	struct StringNode tmpNode;
	tmpNode.next = NULL;
	tmpNode.str = (char *) compare;

	/* in this case we want to return true of the string compare is found in luInfo */
	return !(pluginsCheckInternal ((char *) luInfo, &tmpNode));
}


// C++: plugins.cpp[139-151]
/** @returns Number of storage plugins (should be exactly one) */
bool pluginsCheckStorage (struct Plugins * plugins, const struct Plugin * plugin)
{
	// C++: if (plugin.findInfo ("storage", "provides")) ++nrResolverPlugins;
	if (pluginFindInfo (plugin, "storage", "provides", "infos"))
	{
		(plugins->nrStoragePlugins)++;
	}

	if ((plugins->nrStoragePlugins) > 1)
	{
		(plugins->nrStoragePlugins)--;
		return false;
	}
	else
	{
		return true;
	}
}

// C++: plugins.cpp[153-166]
/** @returns Number of resolver plugins (should be exactly one) */
bool pluginsCheckResolver (struct Plugins * plugins, const struct Plugin * plugin)
{
	// C++: if (plugin.findInfo ("resolver", "provides")) ++nrResolverPlugins;
	if (pluginFindInfo (plugin, "resolver", "provides", "infos"))
	{
		(plugins->nrResolverPlugins)++;
	}

	if ((plugins->nrResolverPlugins) > 1)
	{
		(plugins->nrResolverPlugins)--;
		return false;
	}
	else
	{
		return true;
	}
}

// C++: plugins.cpp[99-104]
/**
 * @brief check if this plugin has at least one placement
 *
 * @param ps the plugin to check
 * @param placement which placementInfo it is
 *
 * @retval true if it should be added
 * @retval false no placements (will not be added)
 */
bool pluginCheckPlacement (const struct Plugin * p, const char * placement)
{
	// C++: if (!plugin.findInfo (which, "placements")) return false; // nothing to check, won't be added anyway
	return pluginFindInfo (p, placement, "placements", "infos");
}

bool pluginsTryPlugin (struct Plugins * existingPlugins, const struct Plugin * toTry, enum PluginType pluginType)
{
	if (pluginType == pluginTypeError || pluginType == pluginTypeCommit)
	{
		if (!pluginsCheckOrdering (toTry, existingPlugins->alreadyProvided))
		{
			/* TODO: Handle error */
			fprintf (stderr, "pluginsCheckOrdering returned false!\n");
			return false;
		}

		if (!pluginsCheckConflicts (toTry, existingPlugins->alreadyProvided, existingPlugins->alreadyConflict))
		{
			/* TODO: Handle error */
			fprintf (stderr, "pluginsCheckConflicts returned false!\n");
			return false;
		}
	}

	bool willBeAdded = false;


	switch (pluginType)
	{
	case pluginTypeError:
		willBeAdded |= pluginCheckPlacement (toTry, "prerollback");
		willBeAdded |= pluginCheckPlacement (toTry, "rollback");
		willBeAdded |= pluginCheckPlacement (toTry, "postrollback");
		break;

	case pluginTypeCommit:
		willBeAdded |= pluginCheckPlacement (toTry, "precommit");
		willBeAdded |= pluginCheckPlacement (toTry, "commit");
		willBeAdded |= pluginCheckPlacement (toTry, "postcommit");
		break;

	case pluginTypeGet:
		willBeAdded |= pluginCheckPlacement (toTry, "getresolver");
		willBeAdded |= pluginCheckPlacement (toTry, "pregetstorage");
		willBeAdded |= pluginCheckPlacement (toTry, "getstorage");
		willBeAdded |= pluginCheckPlacement (toTry, "postgetstorage");
		break;

	case pluginTypeSet:
		willBeAdded |= pluginCheckPlacement (toTry, "setresolver");
		willBeAdded |= pluginCheckPlacement (toTry, "presetstorage");
		willBeAdded |= pluginCheckPlacement (toTry, "setstorage");
		break;

	default:
		return false;
	}

	if (!willBeAdded) return true;

	const char * symbolName = pluginTypeToStr (pluginType);
	if (!getSymbolFromList (toTry->symbols, symbolName))
	{
		// C++: throw MissingSymbol ("error", plugin.name ());
		fprintf (stderr, "Missing symbol: %s, from plugin: %s", symbolName, toTry->ps.name);
		return false;
	}


	if (pluginType == pluginTypeGet || pluginType == pluginTypeSet)
	{
		// C++: checkStorage (plugin);
		if (!pluginsCheckStorage(existingPlugins, toTry))
		{
			/* TODO: Handle error */
			fprintf (stderr, "There must be exactly one storage plugin!\n");
			return false;
		}
	}

	// C++: checkResolver (plugin);
	if (!pluginsCheckResolver (existingPlugins, toTry))
	{
		/* TODO: Handle error */
		fprintf (stderr, "There must be exactly one resolver plugin!\n");
		return false;
	}

	/* all checks ok */
	return true;
}


// C++: plugins.cpp[32-71]
void pluginsAddInfo (struct Plugins * existingPlugins, struct Plugin * plugin)
{
	const char * luInfo;
	for (char i = 0; i < 4; i++)
	{
		struct StringNode * lastNode;
		switch (i)
		{
		case 0:
			luInfo = pluginLookupInfo (plugin, "provides", "infos");
			lastNode = existingPlugins->alreadyProvided;
			break;

		case 1:
			luInfo = pluginLookupInfo (plugin, "needs", "infos");
			lastNode = existingPlugins->needed;
			break;

		case 2:
			luInfo = pluginLookupInfo (plugin, "recommends", "infos");
			lastNode = existingPlugins->recommended;
			break;

		case 3:
			luInfo = pluginLookupInfo (plugin, "conflicts", "infos");
			lastNode = existingPlugins->alreadyConflict;
			break;

		default:
			assert (false);
			break;
		}

		/* forward to last node */
		for (; lastNode->next; lastNode = lastNode->next);



		for (const char * c = luInfo; ; c++)
		{
			if (isspace (*c) || *c == '\0')
			{
				/* does not include separator (space or \0) */
				ptrdiff_t curStrLen = c - luInfo;

				struct StringNode * newStringNode = elektraMalloc (sizeof (struct StringNode));
				newStringNode->next = NULL;
				newStringNode->shouldFreeStr = true;
				newStringNode->str = elektraMalloc (curStrLen + 1);

				/* \0 gets added by strncpy */
				strncpy (newStringNode->str, luInfo, curStrLen + 1);

				/* add to and of list */
				lastNode->next = newStringNode;
				lastNode = lastNode->next; /* new last node */

				if (*c == '\0')
				{
					break;
				}
				else
				{
					/* set to first char of next word */
					luInfo = c + 1;
				}
			}
		}

		/* iteration for 'provides' */
		if (i == 0)
		{
			/* Add the name of the plugin itself */
			struct StringNode * newStringNode = elektraMalloc (sizeof (struct StringNode));
			newStringNode->shouldFreeStr = false;
			newStringNode->str = plugin->ps.name;
			lastNode->next = newStringNode;
		}
	}
}



// C++: plugin.cpp[384-402]
KeySet * pluginGetNeededConfig (const struct Plugin * plugin)
{
	/* TODO: replace string literal with constant */
	Key * keyNeededConfig = keyNew ("system:/elektra/modules", KEY_END);

	// C++: neededConfigKey.addName (spec.getName ());
	keyAddName (keyNeededConfig, plugin->ps.name);
	keyAddName (keyNeededConfig, "config/needs");

	KeySet * dupKsInfo = ksDup (plugin->ksInfo);
	KeySet * ksConfig = ksCut (dupKsInfo, keyNeededConfig);
	ksDel (dupKsInfo);

	KeySet * ksRet = ksNew (ksGetSize (ksConfig), KS_END);
	Key * keyNewParent = keyNew ("system:/", KEY_END);

	for (elektraCursor it = 0; it < ksGetSize (ksConfig); it++)
	{
		// C++: Key k (i->dup ());
		Key * k = keyDup (ksAtCursor (ksConfig, it), KEY_CP_ALL);

		// C++: ret.append (kdb::tools::helper::rebaseKey (k, oldParent, newParent));
		keyReplacePrefix (k, keyNeededConfig, keyNewParent);
		ksAppendKey (ksRet, k);
	}

	ksDel (ksConfig);
	keyDel (keyNeededConfig);
	keyDel (keyNewParent);
	return ksRet;
}


// C++: plugins.cpp[73-88]
bool pluginsAddPlugin (struct Plugins * existingPlugins, struct Plugin * plugin, const char * placement)
{
	if (!pluginFindInfo (plugin, placement, "placements", "infos"))
	{
		fprintf (stderr, "Could not find placement %s for plugin: %s\n", placement, plugin->ps.name);
		return false;
	}

	const char * stacking = pluginLookupInfo (plugin, "stacking", "infos");

	/* Get list of plugins from existingPlugins for the given placement */
	for (struct Plugins_PluginList * curPluginListNode = existingPlugins->pluginList; curPluginListNode; curPluginListNode = curPluginListNode->next)
	{
		if (elektraStrCmp (curPluginListNode->slot, placement) == 0)
		{
			struct PluginNode * newNode = elektraMalloc (sizeof (struct PluginNode));
			newNode->plugin = *plugin;

			if (!(*stacking) && elektraStrCmp (placement, "postgetstorage") == 0)
			{
				/* add plugin to start of list */
				newNode->next = curPluginListNode->plugins;
				curPluginListNode->plugins = newNode;
			}
			else
			{
				/* add plugin to end of list */
				newNode->next = NULL;
				struct PluginNode * curPluginNode;
				for (curPluginNode = curPluginListNode->plugins; curPluginNode->next; curPluginNode = curPluginNode->next);
				curPluginNode->next = newNode;
			}
		}
	}

	return true;
}


// C++: backend.cpp[277-294]
/* Load a plugin based on the provided ps and add it to pluginList */
struct Plugin * backendTryPlugin (struct PluginSpec ps, struct PluginNode * pluginList, struct Plugins * errorPlugins, struct Plugins * commitPlugins, struct Plugins * getPlugins,
	struct Plugins * setPlugins, KeySet * ksModules)
{
	// C++: PluginPtr plugin = modules.load (spec);
	bool errOccurred;
	struct Plugin * plRet = elektraMalloc (sizeof (struct Plugin));
	*plRet = loadPluginFromSpec (ps, ksModules, &errOccurred);

	if (errOccurred)
	{
		freePlugin (true, *plRet, ps);
		elektraFree (plRet);
		return NULL;
	}

	// C++: errorplugins.tryPlugin (*plugin.get ());
	if (!pluginsTryPlugin (errorPlugins, plRet, pluginTypeError)) return NULL;

	// C++: commitplugins.tryPlugin (*plugin.get ());
	if (!pluginsTryPlugin (commitPlugins, plRet, pluginTypeCommit)) return NULL;

	// C++: getplugins.tryPlugin (*plugin.get ());
	if (!pluginsTryPlugin (getPlugins, plRet, pluginTypeGet)) return NULL;

	// C++: setplugins.tryPlugin (*plugin.get ());
	if (!pluginsTryPlugin (setPlugins, plRet, pluginTypeSet)) return NULL;


	/* Check if plugin was already inserted */
	for (struct PluginNode * curNode = pluginList; curNode; curNode = curNode->next)
	{
		char * curNodeFullName = getPluginFullName (curNode->plugin.ps);
		char * plFullName = getPluginFullName (plRet->ps);

		if (elektraStrCmp (curNodeFullName, plFullName) == 0)
		{
			/* TODO: error handling */
			// C++: throw PluginAlreadyInserted (plugin->getFullName ());
			fprintf (stderr, "Plugin already inserted: %s\n", plFullName);
			freePlugin (true, *plRet, ps);
			elektraFree (curNodeFullName);
			elektraFree (plFullName);
			elektraFree (plRet);
			return NULL;
		}

		elektraFree (curNodeFullName);
		elektraFree (plFullName);
	}

	return plRet;
}


// C++: from backend.cpp[310-322]
/**
 * Add a plugin that can be loaded, meets all
 * constraints.
 *
 * @note that this does not mean that the backend
 * validates after it is added. It only means that
 * the situation is not getting worse.
 *
 * For validation @see validated().
 */
struct Plugin * backendAddPlugin (struct PluginSpec ps, struct PluginNode * pluginList, struct Plugins * errorPlugins, struct Plugins * commitPlugins,
	struct Plugins * getPlugins, struct Plugins * setPlugins, KeySet * ksModules, KeySet * ksConfig)
{
	// C++: tryPlugin (plugin);
	struct Plugin * newPlugin = backendTryPlugin (ps, pluginList, errorPlugins, commitPlugins, getPlugins, setPlugins, ksModules);

	if (!newPlugin)
	{
		fprintf (stderr, "Trying the plugin %s failed, could not add plugin!\n", ps.name);
		return NULL;
	}


	// C++: commitplugins.addPlugin (*plugins.back ());
	pluginsAddPlugin (commitPlugins, newPlugin, "precommit");
	pluginsAddPlugin (commitPlugins, newPlugin, "commit");
	pluginsAddPlugin (commitPlugins, newPlugin, "postcommit");
	pluginsAddInfo (commitPlugins, newPlugin);

	// C++: errorplugins.addPlugin (*plugins.back ());
	pluginsAddPlugin (errorPlugins, newPlugin, "prerollback");
	pluginsAddPlugin (errorPlugins, newPlugin, "rollback");
	pluginsAddPlugin (errorPlugins, newPlugin, "postrollback");
	pluginsAddInfo (errorPlugins, newPlugin);

	// C++: getplugins.addPlugin (*plugins.back ());
	pluginsAddPlugin (getPlugins, newPlugin, "getresolver");
	pluginsAddPlugin (getPlugins, newPlugin, "pregetstorage");
	pluginsAddPlugin (getPlugins, newPlugin, "getstorage");
	pluginsAddPlugin (getPlugins, newPlugin, "postgetstorage");

	// C++: setplugins.addPlugin (*plugins.back ());
	pluginsAddPlugin (setPlugins, newPlugin, "setresolver");
	pluginsAddPlugin (setPlugins, newPlugin, "presetstorage");
	pluginsAddPlugin (setPlugins, newPlugin, "setstorage");

	// C++: KeySet toAdd = plugins.back ()->getNeededConfig ();
	KeySet * ksToAdd = pluginGetNeededConfig (newPlugin);
	ksAppend (ksConfig, ksToAdd);

	return newPlugin;
}


/**@pre: resolver needs to be loaded first
 * Will check the filename and use it as configFile for this backend. */
// C++: backend.cpp[246-274], void Backend::useConfigFile (std::string file)
bool backendCheckConfigFile (const char * file, const struct PluginNode * pluginList)
{
	int (*checkFileFunction) (const char *) = NULL;

	/* Get checkfile function from plugin symbols */
	for (; pluginList; pluginList = pluginList->next)
	{
		for (struct PluginSymbols * curSymbol = pluginList->plugin.symbols; curSymbol; curSymbol = curSymbol->next)
		{
			if (elektraStrCmp (curSymbol->symbolName, "checkfile") == 0)
			{
				checkFileFunction = (int (*) (const char *)) curSymbol->symbolFunc;
				break;
			}
		}
	}

	if (!checkFileFunction)
	{
		// C++: throw MissingSymbol ("No resolver with checkfile found", "");
		fprintf (stderr, "No resolver with checkfile found.\n");
		return false;
	}

	int res = checkFileFunction (file);

	if (res == -1)
	{
		// C++: throw FileNotValidException ();
		fprintf (stderr, "FileNotValidException\n");
		return false;
	}

	return true;
}


// C++: backendbuilder.cpp[580-597]
/*TODO: rename */
bool mountBackendBuilderUseConfigFile (const char * file, const struct PluginSpecNode * psn, struct PluginNode ** pluginList,
	struct Plugins * errorPlugins, struct Plugins * commitPlugins, struct Plugins * getPlugins, struct Plugins * setPlugins,
	KeySet * ksModules, KeySet * ksConfig)
{

	if (!pluginList)
	{
		fprintf (stderr, "Got NULL for pluginList, therefore plugins can't be added!\n");
		return false;
	}

	struct PluginNode * lastNode;
	for (lastNode = *pluginList; lastNode->next; lastNode = lastNode->next);



	// C++: MountBackendInterfacePtr b = getBackendFactory ().create ();
	for (const struct PluginSpecNode * curPs = psn; curPs; curPs = curPs->next)
	{
		char * const luInfo = modulesPluginDatabaseLookupInfo (curPs->ps, "provides", ksModules);
		if (elektraStrCmp (luInfo, "resolver") == 0)
		{
			elektraFree (luInfo);
			// C++: fillPlugins (*b)
			for (; psn; psn = psn->next)
			{
				struct Plugin * newPlugin = backendAddPlugin (psn->ps, *pluginList , errorPlugins, commitPlugins, getPlugins, setPlugins, ksModules, ksConfig);

				if (newPlugin)
				{
					struct PluginNode * newNode = elektraMalloc (sizeof (struct PluginNode));
					newNode->plugin = *newPlugin;
					newNode->next = NULL;

					/* only free the outer struct, pointers inside are still as before and now accessible via newNode->plugin */
					elektraFree (newPlugin);

					if (lastNode)
					{
						lastNode->next = newNode;
					}
					else
					{
						/* empty list given --> newNode is start of list */
						*pluginList = newNode;
					}

					lastNode = newNode;
				}
			}

			// C++: b->useConfigFile (configfile);
			/* Check for resolver with "checkfile" */
			return backendCheckConfigFile (file, *pluginList);
		}
		else
		{
			elektraFree (luInfo);
		}
	}

	/* accept file */
	return true;
}



/*****************************************************
 * PART VI: Serialization of config                  *
 *****************************************************/

// C++: from plugins.cpp[394-411]
bool pluginsSerializeConfig (const char * name, const KeySet * ksSrc, KeySet * ksDest)
{
	if (ksGetSize (ksSrc) <= 0) return false;
	if (!ksDest) return false;

	Key * oldParentKey = keyNew ("user:/", KEY_END);

	size_t newParentNameSize = elektraStrLen (name) + elektraStrLen ("/config") - 1;
	char * newParentName = elektraMalloc (newParentNameSize);
	strcpy (newParentName, name);
	strcat (newParentName, "/config");
	Key * newParentKey = keyNew (newParentName, KEY_END);
	elektraFree (newParentName);

	ksAppendKey (ksDest, newParentKey);

	for (elektraCursor i = 0; i < ksGetSize (ksSrc); i++)
	{
		Key * curKey = ksAtCursor (ksSrc, i);

		if (keyGetNamespace (curKey) == KEY_NS_USER)
		{
			curKey = keyDup (curKey, KEY_CP_ALL);
			keyReplacePrefix (curKey, oldParentKey, newParentKey);
			ksAppendKey (ksDest, curKey);
		}
	}

	keyDel (oldParentKey);
	keyDel (newParentKey);
	return true;
}

// C++: from plugins.cpp[415-606]
void serializePlugins (const Key * baseKey, KeySet * ksMountConf, struct Plugins * pluginsToProcess, enum PluginType pluginType)
{
	if (!baseKey || !ksMountConf || !pluginsToProcess)
	{
		fprintf (stderr, "serializePlugins () was called with at least one NULL-argument!\n");
		return;
	}

	for (struct Plugins_PluginList * pluginList = pluginsToProcess->pluginList; pluginList; pluginList = pluginList->next)
	{
		/* TODO: Check if "commit" should be replaced instead of "get" (maybe an error in the c++ code) */
		/* remove "get" from string */
		size_t oldRoleLen = elektraStrLen (pluginList->slot);
		char * roleName = elektraMalloc (oldRoleLen);
		strcpy (roleName, pluginList->slot);


		/* don't remove name parts from error plugins */
		for (size_t i = 0; pluginType != pluginTypeError && i < oldRoleLen - 1; i++)
		{
			/* TODO: Check if handling of commit plugins is correct */
			/* check for "get" or "set" (commit-plugins also use get in C++ code) */
			if (roleName[i] == (pluginType == pluginTypeSet ? 's' : 'g') && roleName[i+1] == 'e' && roleName[i+2] == 't')
			{
				/* move remaining part of string forward */
				size_t j;
				for (j = i + 3; j < oldRoleLen - 1; j++)
				{
					roleName[j-3] = roleName[j];
				}
				roleName[oldRoleLen-1] = roleName[oldRoleLen-2] = roleName[oldRoleLen-3] = '\0';
				oldRoleLen -= 3;
			}
		}

		for (struct PluginNode * curNode = pluginList->plugins; curNode; curNode = curNode->next)
		{
			size_t refKeyNameSize = keyGetNameSize (baseKey) + elektraStrLen ("/plugins/") + elektraStrLen (curNode->plugin.ps.refname) - 2;
			char * refKeyName = elektraMalloc (refKeyNameSize);
			keyGetName (baseKey, refKeyName, refKeyNameSize);
			strcat (refKeyName, "/plugins/");
			strcat (refKeyName, curNode->plugin.ps.refname);
			Key * refKey = keyNew (refKeyName, KEY_END);

			if (!ksLookup (ksMountConf, refKey, KDB_O_NONE))
			{
				ksAppendKey (ksMountConf, refKey);

				size_t refKeyNameNameSize = keyGetNameSize (refKey) + elektraStrLen ("/name") - 1;
				char * refKeyNameName = elektraMalloc (refKeyNameNameSize);
				keyGetName (refKey, refKeyNameName, refKeyNameNameSize);
				keyDel (refKey);
				strcat (refKeyNameName, "/name");


				refKey = keyNew (refKeyNameName, KEY_VALUE, curNode->plugin.ps.name, KEY_END);
				elektraFree (refKeyNameName);
				ksAppendKey (ksMountConf, refKey);

				pluginsSerializeConfig (refKeyName, curNode->plugin.ps.config, ksMountConf);
			}
			keyDel (refKey);
			elektraFree (refKeyName);

			size_t positionKeyNameSize = keyGetNameSize (baseKey) + elektraStrLen ("/definition/positions/set/")
				+ elektraStrLen (roleName) + 1; /* we have 3 bytes for storing "/#0" */
			char * positionKeyName = elektraMalloc (positionKeyNameSize);
			keyGetName (baseKey, positionKeyName, positionKeyNameSize);

			bool multiplePluginsAllowed;
			switch (pluginType)
			{
			case pluginTypeGet:
				multiplePluginsAllowed = (elektraStrCmp (roleName, "prestorage") == 0 || elektraStrCmp (roleName, "poststorage") == 0);
				strcat (positionKeyName, "/definition/positions/get/");
				break;
			case pluginTypeSet: /* fall through */
			case pluginTypeError:
				multiplePluginsAllowed = (elektraStrCmp (roleName, "prestorage") == 0 || elektraStrCmp (roleName, "poststorage") == 0);
				strcat (positionKeyName, "/definition/positions/set/");
				break;
			case pluginTypeCommit:
				multiplePluginsAllowed = (elektraStrCmp (roleName, "precommit") == 0 || elektraStrCmp (roleName, "postcommit") == 0);
				strcat (positionKeyName, "/definition/positions/set/");
				break;
			default:
				multiplePluginsAllowed = false;
			}

			strcat (positionKeyName, roleName);

			if (multiplePluginsAllowed)
			{
				strcat (positionKeyName, "/#0");
				Key * positionKey = keyNew (positionKeyName, KEY_VALUE, curNode->plugin.ps.refname, KEY_END);
				elektraFree (positionKeyName);

				while (ksLookup (ksMountConf, positionKey, KDB_O_NONE))
				{
					elektraArrayIncName (positionKey);
				}
				ksAppendKey (ksMountConf, positionKey);
			}
			else
			{
				Key * positionKey = keyNew (positionKeyName, KEY_VALUE, curNode->plugin.ps.refname, KEY_END);
				elektraFree (positionKeyName);

				if (ksLookup (ksMountConf, positionKey, KDB_O_NONE))
				{
					 /* TODO: Handle error */
					 fprintf (stderr, "Too many plugins! Position %s/%s can only contain a single plugin.",
					 	pluginType == pluginTypeGet ? "get" : "set", roleName);
					 elektraFree (roleName);
					 keyDel (positionKey);
					 return;
				}
				ksAppendKey (ksMountConf, positionKey);
			}
		}
		elektraFree (roleName);
	}
}

// C++ backendbuilder.cpp[604-612] - void MountBackendBuilder::serialize (kdb::KeySet & ret)
/* returns filled ksMountConf KeySet */
void serialize (KeySet * ksMountConf, const char * mp, const char * configFile, const struct PluginNode * pluginList,
	struct Plugins * commitPlugins, struct Plugins * errorPlugins, struct Plugins * getPlugins, struct Plugins * setPlugins,
	KeySet * ksConfig)
{
	// C++ backend.cpp[398]
	assert (mp && *mp);

	/* Check config file */
	if (!configFile || !(*configFile))
	{
		/* TODO: handle error */
		return;
	}

	if (!backendCheckConfigFile (configFile, pluginList))
	{
		/* TODO: Handle error */
		fprintf (stderr, "Check of configfile failed!\n");
		return;
	}


	char * mpBasePath = getBasePath (mp);

	if (!mpBasePath || !(*mpBasePath))
	{
		/* TODO: Handle error */
		fprintf (stderr, "Could not get basepath from mountpoint!\n");
		return;
	}

	if (ksLookupByName (ksMountConf, mpBasePath, KDB_O_NONE))
	{
		fprintf (stderr, "Mountpoint already in use!\n");
		elektraFree (mpBasePath);
		return;
	}


	Key * backendRootKey = keyNew (mpBasePath, KEY_END);

	/* TODO: serialize plugins (commit, error, get, set) */
	serializePlugins (backendRootKey, ksMountConf, commitPlugins, pluginTypeCommit);
	serializePlugins (backendRootKey, ksMountConf, errorPlugins, pluginTypeError);
	serializePlugins (backendRootKey, ksMountConf, getPlugins, pluginTypeGet);
	serializePlugins (backendRootKey, ksMountConf, setPlugins, pluginTypeSet);

	/* Append additional keys to the mountConf */
	size_t nameBufSize = elektraStrLen (mpBasePath);
	if (keyGetNameSize(backendRootKey) > (ssize_t) nameBufSize)
	{
		nameBufSize = keyGetNameSize (backendRootKey);
	}
	nameBufSize += elektraStrLen ("/plugins/backend/name") - 1; /* do not include size for \0 a 2nd time */

	char * nameBuf = elektraMalloc (nameBufSize);
	char * baseEnd = stpcpy (nameBuf, mpBasePath);

	strcpy (baseEnd, "/plugins/backend");
	Key * backendKey = keyNew (nameBuf, KEY_END);
	ksAppendKey (ksMountConf, backendKey);

	strcpy (baseEnd, "/plugins/backend/name");
	Key * backendNameKey = keyNew (nameBuf, KEY_VALUE, "backend", KEY_END);
	ksAppendKey (ksMountConf, backendNameKey);

	keyGetName (backendRootKey, nameBuf, nameBufSize);
	strcat (nameBuf, "/definition/path");
	Key * pathKey = keyNew (nameBuf, KEY_VALUE, configFile, KEY_END);
	ksAppendKey (ksMountConf, pathKey);

	strcpy (nameBuf, mpBasePath);
	strcpy (baseEnd, "/config");
	Key * configKey = keyNew (nameBuf, KEY_END);
	ksAppendKey (ksMountConf, configKey);


	Key * oldParentKey = keyNew ("system:/", KEY_END);
	Key * newParentKey = keyNew (nameBuf, KEY_END);
	elektraFree (nameBuf);

	for (elektraCursor i = 0; i < ksGetSize (ksConfig); i++)
	{
		Key * curKey = keyDup (ksAtCursor (ksConfig, i), KEY_CP_ALL);

		/* TODO: Check return value for error */
		keyReplacePrefix (curKey, oldParentKey, newParentKey);

		ksAppendKey (ksMountConf, curKey);
	}

	ksAppendKey (ksMountConf, backendRootKey);
}


/**************************************************************************
 * PART VII: Start-function that should be called from tools / other code *
 **************************************************************************/

void cBuildBackend (KeySet * const ksMountConf, const char * const mountPoint, char * pluginsConfig, bool clForce, bool clDebug,
		    int mergeStrategy, char * resolverName, const char * path, const KeySet * ksPlugins, bool withRecommends)
{
	if (!path || !(*path))
	{
		/* TODO: Handle error */
		fprintf (stderr, "Mounting error: No path for mointpoint given!\n");
		return;
	}

	Key * const keyMountpoint = keyNew (mountPoint, KEY_END);
	if (!keyMountpoint)
	{
		/* TODO: Implement error handling
		 * throw invalid_argument (mp + " is not a valid mountpoint"); */
		fprintf (stderr, "Mounting error: %s is not a valid mountpoint!\n", mountPoint);
		return;
	}

	/* TODO: needed for strategy="unchanged", check if that should be supported by the new mounting lib */
	//const KeySet * ksDupMountConf = ksDup (ksMountConf);

	/* TODO: Strategy was "!=preserve" in cpp-code, check merging for mounting */
	if (clForce || mergeStrategy != MERGE_STRATEGY_ABORT)
	{
		Key * const cutKey = keyNew (DEFAULT_MOUNTPOINTS_PATH, KEY_END);
		keyAddBaseName (cutKey, keyName (keyMountpoint));
		KeySet * ksTmp = ksCut (ksMountConf, cutKey);
		/* We don't need the cut-out KeySet, but only the changed mountconf KeySet */
		ksDel (ksTmp);
		keyDel (cutKey);
	}

	/* in C++: backend.setMountpoint (mpk, mountConf); */
	if (!isValidMountPoint (keyMountpoint, ksMountConf))
	{
		/* TODO: error handling */
		keyDel (keyMountpoint);
		fprintf (stderr, "Mounting error: %s is not a valid mountpoint!\n", keyName (keyMountpoint));
		return;
	}
	else
	{
		keyDel (keyMountpoint);
	}

	/* pluginsConfig is a cmd-line parameter (NULL if not provided) */
	/* in C++: backend.setBackendConfig (cl.getPluginsConfig ("system:/")); */
	KeySet * ksBackendConfig = cParsePluginArguments (pluginsConfig, "system:");

	if (clDebug)
	{
		/* TODO: replace with logging */
		printf ("Trying to load the resolver plugin %s\n", resolverName);
	}

	/* in C++: PluginSpec resolver (cl.resolver); (Constructor calls setFullName) */
	struct PluginSpec psResolver;
	psResolver.config = NULL;

	/* Dup string here, because the memory may gets freed and the string gets replaced.
	 * As the string resolverName was an argument of the function, it should not be changed or deleted! */
	if (!setPluginFullName (&psResolver, elektraStrDup (resolverName)))
	{
		/* TODO: error handling */
		fprintf (stderr, "Could not set full plugin name for resolver plugin!\n");
		return;
	}

	/* from modules.hpp */
	KeySet * ksModules;

	/* 2nd parameter is unused in function implementation, therefore we use NULL here */
	elektraModulesInit (ksModules, NULL);


	/* in C++: backend.addPlugin (PluginSpec (resolver));
	 * -> moved to own function */
	struct PluginSpec psToAdd = backendBuilderAddPlugin (NULL, psResolver, &ksBackendConfig, ksModules);
	/* C++ end of backend.addPlugin */

	struct PluginSpecNode * pluginSpecsToAdd = elektraMalloc (sizeof (struct PluginSpecNode));
	pluginSpecsToAdd->ps = psToAdd;
	pluginSpecsToAdd->next = NULL;

	struct PluginNode * pluginList = NULL;
	struct Plugins errorPlugins, commitPlugins, getPlugins, setPlugins;

	/* the global config, plugins might add something to it */
	KeySet * ksConfig = ksNew (0, KS_END);



	/* C++: backend.useConfigFile (path) */
	if (false == mountBackendBuilderUseConfigFile (path, pluginSpecsToAdd, &pluginList, &errorPlugins, &commitPlugins, &getPlugins, &setPlugins, ksModules, ksConfig))
	{
		fprintf (stderr, "Could not add resolver plugin!\n");

		/* free list nodes and plugins specs */
		freePsList (pluginSpecsToAdd, true);

		struct PluginSpec psNull = {NULL, NULL, NULL};
		while (pluginList)
		{
			/* clear all members that are not null */
			freePlugin (true, pluginList->plugin, psNull);

			struct PluginNode * nextNode = pluginList->next;
			elektraFree (pluginList);
			pluginList = nextNode;
		}

		freePlugins (&errorPlugins, false);
		freePlugins (&commitPlugins, false);
		freePlugins (&getPlugins, false);
		freePlugins (&setPlugins, false);
		elektraModulesClose (ksModules, NULL);
		ksDel (ksConfig);
		ksDel (ksModules);
		ksDel (ksBackendConfig);

		return;
	}

	if (clDebug)
	{
		printf ("Trying to add default plugins:");
		for (elektraCursor it = 0; it < ksGetSize (ksPlugins); it++)
		{
			printf (" %s", keyString (ksAtCursor (ksPlugins, it)));
		}
	}

	/* Convert KeySet to String */
	size_t pluginsStrLen = 0;
	for (elektraCursor it = 0; it < ksGetSize (ksPlugins); it++)
	{
		/* Byte for '\0' is counted and needed for space in result string */
		pluginsStrLen += keyGetValueSize(ksAtCursor(ksPlugins, it));
	}

	char * pluginsStr = elektraMalloc(pluginsStrLen);
	char * pluginStrCurPos = pluginsStr;
	for (elektraCursor it = 0; it < ksGetSize (ksPlugins); it++)
	{
		if (pluginStrCurPos > pluginsStr)
		{
			*pluginStrCurPos  = ' ';
			pluginStrCurPos++;
		}

		pluginStrCurPos = stpcpy (pluginStrCurPos, keyString(ksAtCursor(ksPlugins, it)));
	}

	/* C++: backend.addPlugins (parseArguments (cl.plugins)); */
	struct PluginSpecNode * pluginSpecList = parseArguments (pluginsStr);
	/* parseArguments creates a copy of the string, if needed, so it can safely be freed now */
	elektraFree (pluginsStr);

	for (struct PluginSpecNode * curNode = pluginSpecList; curNode; curNode = curNode->next)
	{
		/* add new plugin and sort list pluginSpecsToAdd */
		pluginSpecsToAdd = backendBuilderAddAndInsertPlugin (pluginSpecsToAdd, curNode->ps, &ksBackendConfig, ksModules);
	}

	 /* Free the actual nodes */
	 freePsList (pluginSpecList, false);

	/* TODO: Check if needed anymore (new handling of command arguments with gopts) */
	/* C++: const size_t nonPlugins = 2;
		backend.addPlugins (parseArguments (cl.arguments.begin () + nonPlugins, cl.arguments.end ())); */


	/* Call it a day */


	/* C++: backend.needPlugin ("storage");
	 * 	backend.needPlugin ("sync"); */
	struct StringNode * neededPlugins = addStrAtEnd (NULL, "storage", false);
	addStrAtEnd (neededPlugins, "sync", false);

	// C++: outputMissingRecommends (backend.resolveNeeds (cl.withRecommends));
	struct StringNode * missingRecommends = resolveNeeds (withRecommends, pluginSpecsToAdd, neededPlugins, NULL, NULL, &ksBackendConfig, ksModules);

	/* CAUTION: The pointer "neededPlugins" may points to invalid memory (freed or part of  the list 'pluginSpecsToAdd')
	 * So you must not dereference it after the call of 'resolveNeeds (...)'! */
	neededPlugins = NULL;

	if (missingRecommends && missingRecommends->str)
	{
		printf ("Missing recommended plugins: ");
		for (struct StringNode * curNode = missingRecommends; curNode; curNode = curNode->next)
		{
			printf ("%s ", curNode->str);
		}
		printf ("\n");
	}
	freeStringList (missingRecommends);


	// C++: backend.serialize (mountConf);
	serialize (ksMountConf, mountPoint, path, pluginList, &commitPlugins, &errorPlugins, &getPlugins, &setPlugins, ksConfig);


	struct PluginSpec psNull = {NULL, NULL, NULL};
	while (pluginList)
	{
		/* clear all members that are not null */
		freePlugin (true, pluginList->plugin, psNull);

		struct PluginNode * nextNode = pluginList->next;
		elektraFree (pluginList);
		pluginList = nextNode;
	}

	freePlugins (&errorPlugins, false);
	freePlugins (&commitPlugins, false);
	freePlugins (&getPlugins, false);
	freePlugins (&setPlugins, false);
	freePsList (pluginSpecsToAdd, true);

	ksDel (ksBackendConfig);
	elektraModulesClose (ksModules, NULL);
	ksDel (ksModules);

	ksDel (ksMountConf);
	ksDel (ksConfig);

	/* TODO: Should cl.strategy == "unchanged" be supported? (like in legecy cpp-code) */
}

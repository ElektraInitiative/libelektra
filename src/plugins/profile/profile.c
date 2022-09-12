/**
 * @file
 *
 * @brief Source for profile plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "profile.h"

#include <kdbhelper.h>

#include <kdb.h>     //actual namespaces
#include <kdbease.h> //elektraKeyGetRelativeName
#include <kdbos.h>   //elektraNamespace
#include <stdio.h>
#include <string.h>

#include <fnmatch.h>

#define PROFILEPATH "*/sw/*/*/#*/profile"
#define CURRENTPATH "*/sw/*/*/#*/current"

int elektraProfileOpen (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	ElektraKeyset * appendedKeys = NULL;
	elektraPluginSetData (handle, appendedKeys);
	return 1; // success
}

int elektraProfileClose (Plugin * handle ELEKTRA_UNUSED, ElektraKey * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	ElektraKeyset * appendedKeys = elektraPluginGetData (handle);
	if (appendedKeys) ksDel (appendedKeys);
	return 1; // success
}

static ElektraKey * keyDupWithNS (const ElektraKey * origKey, elektraNamespace ns)
{
	ElektraKey * newKey = NULL;
	switch (ns)
	{
	case ELEKTRA_NS_SPEC:
		newKey = keyNew ("spec:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_DIR:
		newKey = keyNew ("dir:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_USER:
		newKey = keyNew ("user:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_SYSTEM:
		newKey = keyNew ("system:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_PROC:
		newKey = keyNew ("proc:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_META:
		newKey = keyNew ("meta:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_DEFAULT:
		newKey = keyNew ("default:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_CASCADING:
	default:
		newKey = keyNew ("/", ELEKTRA_KEY_END);
		break;
	}
	const char * relativeName = keyName (origKey);
	char * nextPtr = NULL;
	if (keyName (origKey)[0] == '/')
		++relativeName;
	else if ((nextPtr = strchr (keyName (origKey), '/')))
		relativeName = (++nextPtr);
	keyAddName (newKey, relativeName);
	return newKey;
}

static void linkProfileKeys (ElektraKeyset * swKS, ElektraKeyset * profileKeys, ElektraKeyset * appendedKeys)
{
	ElektraKey * profileCutKey = NULL;
	ElektraKey * profileKey = NULL;
	const char * profileString = NULL;
	ksRewind (profileKeys);
	while ((profileKey = ksNext (profileKeys)) != NULL)
	{
		profileString = keyString (profileKey);
		if (profileString)
		{
			profileCutKey = keyDup (profileKey, ELEKTRA_KEY_CP_ALL);
			keyAddName (profileCutKey, "..");
			ElektraKey * currentProfileKey = keyDup (profileCutKey, ELEKTRA_KEY_CP_ALL);
			keyAddBaseName (currentProfileKey, "current");
			keyAddBaseName (profileCutKey, profileString);
			ElektraKeyset * profileKS = ksCut (swKS, profileCutKey);
			ksRewind (profileKS);
			ElektraKey * cur;
			while ((cur = ksNext (profileKS)) != NULL)
			{
				if (!strcmp (keyName (cur), keyName (profileCutKey))) continue;
				ElektraKey * overrideKey = keyDupWithNS (currentProfileKey, ELEKTRA_NS_SPEC);
				const char * relativeName = elektraKeyGetRelativeName (cur, profileCutKey);
				keyAddName (overrideKey, relativeName);
				ElektraKey * lookupKey = keyDupWithNS (overrideKey, keyGetNamespace (currentProfileKey));
				if (ksLookup (swKS, lookupKey, ELEKTRA_KDB_O_NONE))
				{
					keyDel (lookupKey);
					keyDel (overrideKey);
					continue;
				}
				keyDel (lookupKey);
				keySetMeta (overrideKey, "override/#0", keyName (cur));
				ksAppendKey (swKS, keyDup (overrideKey, ELEKTRA_KEY_CP_ALL));
				ksAppendKey (appendedKeys, keyDup (overrideKey, ELEKTRA_KEY_CP_ALL));
				keyDel (overrideKey);
			}
			keyDel (currentProfileKey);
			keyDel (profileCutKey);
			ksAppend (swKS, profileKS);
			ksDel (profileKS);
		}
	}
}

static void linkDefaultKeys (ElektraKeyset * swKS, ElektraKeyset * profileParents, ElektraKeyset * appendedKeys)
{
	ksRewind (profileParents);
	ElektraKey * profileParent = NULL;
	while ((profileParent = ksNext (profileParents)) != NULL)
	{
		ElektraKey * defaultCutKey = keyDup (profileParent, ELEKTRA_KEY_CP_ALL);
		keyAddName (defaultCutKey, "%");
		ElektraKeyset * defaultKS = ksCut (swKS, defaultCutKey);
		ksRewind (defaultKS);
		ElektraKey * cur;
		ElektraKey * currentProfileKey = keyDup (profileParent, ELEKTRA_KEY_CP_ALL);
		keyAddName (currentProfileKey, "current");
		while ((cur = ksNext (defaultKS)) != NULL)
		{
			if (!strcmp (keyName (cur), keyName (defaultCutKey))) continue;
			const char * relativeName = elektraKeyGetRelativeName (cur, defaultCutKey);
			ElektraKey * overrideKey = keyDupWithNS (currentProfileKey, ELEKTRA_NS_SPEC);
			keyAddName (overrideKey, relativeName);
			ElektraKey * existingKey = keyDupWithNS (overrideKey, keyGetNamespace (profileParent));
			if (ksLookup (swKS, overrideKey, ELEKTRA_KDB_O_NONE) || ksLookup (swKS, existingKey, ELEKTRA_KDB_O_NONE))
			{
				keyDel (overrideKey);
				keyDel (existingKey);
				continue;
			}
			keyDel (existingKey);
			keySetMeta (overrideKey, "override/#0", keyName (cur));
			ksAppendKey (swKS, keyDup (overrideKey, ELEKTRA_KEY_CP_ALL));
			ksAppendKey (appendedKeys, keyDup (overrideKey, ELEKTRA_KEY_CP_ALL));
			keyDel (overrideKey);
		}
		keyDel (currentProfileKey);
		keyDel (defaultCutKey);
		ksAppend (swKS, defaultKS);
		ksDel (defaultKS);
	}
}

int elektraProfileGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/profile"))
	{
		ElektraKeyset * contract =
			ksNew (30, keyNew ("system:/elektra/modules/profile", ELEKTRA_KEY_VALUE, "profile plugin waits for your orders", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports", ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports/open", ELEKTRA_KEY_FUNC, elektraProfileOpen, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports/close", ELEKTRA_KEY_FUNC, elektraProfileClose, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports/get", ELEKTRA_KEY_FUNC, elektraProfileGet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports/set", ELEKTRA_KEY_FUNC, elektraProfileSet, ELEKTRA_KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports/error", ELEKTRA_KEY_FUNC, elektraProfileError, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/profile/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	ElektraKey * swKey = keyNew ("/sw", ELEKTRA_KEY_END);
	ElektraKeyset * swKS = ksCut (returned, swKey);
	keyDel (swKey);
	ksRewind (swKS);
	ElektraKey * cur;

	ElektraKeyset * appendedKeys = elektraPluginGetData (handle);
	if (!appendedKeys) appendedKeys = ksNew (0, ELEKTRA_KS_END);
	ElektraKeyset * profileKeys = ksNew (0, ELEKTRA_KS_END);
	while ((cur = ksNext (swKS)) != NULL)
	{
		if (!fnmatch (PROFILEPATH, keyName (cur), FNM_PATHNAME))
		{
			ksAppendKey (profileKeys, cur);
		}
	}
	linkProfileKeys (swKS, profileKeys, appendedKeys);
	ksDel (profileKeys);
	ksDel (appendedKeys);
	ksRewind (swKS);
	ElektraKeyset * profileParents = ksNew (0, ELEKTRA_KS_END);
	while ((cur = ksNext (swKS)) != NULL)
	{
		if (!fnmatch (CURRENTPATH, keyName (cur), FNM_PATHNAME))
		{
			ElektraKey * profileParent = keyDup (cur, ELEKTRA_KEY_CP_ALL);
			keyAddName (profileParent, "..");
			ksAppendKey (profileParents, keyDup (profileParent, ELEKTRA_KEY_CP_ALL));
			keyDel (profileParent);
		}
	}
	linkDefaultKeys (swKS, profileParents, appendedKeys);
	ksDel (profileParents);
	ksAppend (returned, swKS);
	ksDel (swKS);

	return 1; // success
}

int elektraProfileSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	ElektraKeyset * appendedKeys = elektraPluginGetData (handle);
	if (!appendedKeys) return 1;
	ksRewind (appendedKeys);
	ElektraKey * cur;
	while ((cur = ksNext (appendedKeys)) != NULL)
	{
		keyDel (ksLookup (returned, cur, ELEKTRA_KDB_O_POP));
	}
	ksDel (appendedKeys);
	elektraPluginSetData (handle, NULL);
	return 1; // success
}

int elektraProfileError (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// set all keys

	return 1; // success
}

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("profile",
			ELEKTRA_PLUGIN_OPEN,	&elektraProfileOpen,
			ELEKTRA_PLUGIN_CLOSE,	&elektraProfileClose,
			ELEKTRA_PLUGIN_GET,	&elektraProfileGet,
			ELEKTRA_PLUGIN_SET,	&elektraProfileSet,
			ELEKTRA_PLUGIN_ERROR,	&elektraProfileError,
			ELEKTRA_PLUGIN_END);
}


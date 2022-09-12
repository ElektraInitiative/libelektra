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
	if (appendedKeys) elektraKeysetDel (appendedKeys);
	return 1; // success
}

static ElektraKey * keyDupWithNS (const ElektraKey * origKey, elektraNamespace ns)
{
	ElektraKey * newKey = NULL;
	switch (ns)
	{
	case ELEKTRA_NS_SPEC:
		newKey = elektraKeyNew ("spec:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_DIR:
		newKey = elektraKeyNew ("dir:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_USER:
		newKey = elektraKeyNew ("user:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_SYSTEM:
		newKey = elektraKeyNew ("system:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_PROC:
		newKey = elektraKeyNew ("proc:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_META:
		newKey = elektraKeyNew ("meta:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_DEFAULT:
		newKey = elektraKeyNew ("default:/", ELEKTRA_KEY_END);
		break;
	case ELEKTRA_NS_CASCADING:
	default:
		newKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
		break;
	}
	const char * relativeName = elektraKeyName (origKey);
	char * nextPtr = NULL;
	if (elektraKeyName (origKey)[0] == '/')
		++relativeName;
	else if ((nextPtr = strchr (elektraKeyName (origKey), '/')))
		relativeName = (++nextPtr);
	elektraKeyAddName (newKey, relativeName);
	return newKey;
}

static void linkProfileKeys (ElektraKeyset * swKS, ElektraKeyset * profileKeys, ElektraKeyset * appendedKeys)
{
	ElektraKey * profileCutKey = NULL;
	ElektraKey * profileKey = NULL;
	const char * profileString = NULL;
	elektraKeysetRewind (profileKeys);
	while ((profileKey = elektraKeysetNext (profileKeys)) != NULL)
	{
		profileString = elektraKeyString (profileKey);
		if (profileString)
		{
			profileCutKey = elektraKeyDup (profileKey, ELEKTRA_KEY_CP_ALL);
			elektraKeyAddName (profileCutKey, "..");
			ElektraKey * currentProfileKey = elektraKeyDup (profileCutKey, ELEKTRA_KEY_CP_ALL);
			elektraKeyAddBaseName (currentProfileKey, "current");
			elektraKeyAddBaseName (profileCutKey, profileString);
			ElektraKeyset * profileKS = elektraKeysetCut (swKS, profileCutKey);
			elektraKeysetRewind (profileKS);
			ElektraKey * cur;
			while ((cur = elektraKeysetNext (profileKS)) != NULL)
			{
				if (!strcmp (elektraKeyName (cur), elektraKeyName (profileCutKey))) continue;
				ElektraKey * overrideKey = keyDupWithNS (currentProfileKey, ELEKTRA_NS_SPEC);
				const char * relativeName = elektraKeyGetRelativeName (cur, profileCutKey);
				elektraKeyAddName (overrideKey, relativeName);
				ElektraKey * lookupKey = keyDupWithNS (overrideKey, elektraKeyGetNamespace (currentProfileKey));
				if (elektraKeysetLookup (swKS, lookupKey, ELEKTRA_KDB_O_NONE))
				{
					elektraKeyDel (lookupKey);
					elektraKeyDel (overrideKey);
					continue;
				}
				elektraKeyDel (lookupKey);
				elektraKeySetMeta (overrideKey, "override/#0", elektraKeyName (cur));
				elektraKeysetAppendKey (swKS, elektraKeyDup (overrideKey, ELEKTRA_KEY_CP_ALL));
				elektraKeysetAppendKey (appendedKeys, elektraKeyDup (overrideKey, ELEKTRA_KEY_CP_ALL));
				elektraKeyDel (overrideKey);
			}
			elektraKeyDel (currentProfileKey);
			elektraKeyDel (profileCutKey);
			elektraKeysetAppend (swKS, profileKS);
			elektraKeysetDel (profileKS);
		}
	}
}

static void linkDefaultKeys (ElektraKeyset * swKS, ElektraKeyset * profileParents, ElektraKeyset * appendedKeys)
{
	elektraKeysetRewind (profileParents);
	ElektraKey * profileParent = NULL;
	while ((profileParent = elektraKeysetNext (profileParents)) != NULL)
	{
		ElektraKey * defaultCutKey = elektraKeyDup (profileParent, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddName (defaultCutKey, "%");
		ElektraKeyset * defaultKS = elektraKeysetCut (swKS, defaultCutKey);
		elektraKeysetRewind (defaultKS);
		ElektraKey * cur;
		ElektraKey * currentProfileKey = elektraKeyDup (profileParent, ELEKTRA_KEY_CP_ALL);
		elektraKeyAddName (currentProfileKey, "current");
		while ((cur = elektraKeysetNext (defaultKS)) != NULL)
		{
			if (!strcmp (elektraKeyName (cur), elektraKeyName (defaultCutKey))) continue;
			const char * relativeName = elektraKeyGetRelativeName (cur, defaultCutKey);
			ElektraKey * overrideKey = keyDupWithNS (currentProfileKey, ELEKTRA_NS_SPEC);
			elektraKeyAddName (overrideKey, relativeName);
			ElektraKey * existingKey = keyDupWithNS (overrideKey, elektraKeyGetNamespace (profileParent));
			if (elektraKeysetLookup (swKS, overrideKey, ELEKTRA_KDB_O_NONE) || elektraKeysetLookup (swKS, existingKey, ELEKTRA_KDB_O_NONE))
			{
				elektraKeyDel (overrideKey);
				elektraKeyDel (existingKey);
				continue;
			}
			elektraKeyDel (existingKey);
			elektraKeySetMeta (overrideKey, "override/#0", elektraKeyName (cur));
			elektraKeysetAppendKey (swKS, elektraKeyDup (overrideKey, ELEKTRA_KEY_CP_ALL));
			elektraKeysetAppendKey (appendedKeys, elektraKeyDup (overrideKey, ELEKTRA_KEY_CP_ALL));
			elektraKeyDel (overrideKey);
		}
		elektraKeyDel (currentProfileKey);
		elektraKeyDel (defaultCutKey);
		elektraKeysetAppend (swKS, defaultKS);
		elektraKeysetDel (defaultKS);
	}
}

int elektraProfileGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/profile"))
	{
		ElektraKeyset * contract =
			elektraKeysetNew (30, elektraKeyNew ("system:/elektra/modules/profile", ELEKTRA_KEY_VALUE, "profile plugin waits for your orders", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/profile/exports", ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/profile/exports/open", ELEKTRA_KEY_FUNC, elektraProfileOpen, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/profile/exports/close", ELEKTRA_KEY_FUNC, elektraProfileClose, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/profile/exports/get", ELEKTRA_KEY_FUNC, elektraProfileGet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/profile/exports/set", ELEKTRA_KEY_FUNC, elektraProfileSet, ELEKTRA_KEY_END),
			       elektraKeyNew ("system:/elektra/modules/profile/exports/error", ELEKTRA_KEY_FUNC, elektraProfileError, ELEKTRA_KEY_END),
#include ELEKTRA_README
			       elektraKeyNew ("system:/elektra/modules/profile/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

		return 1; // success
	}
	// get all keys
	ElektraKey * swKey = elektraKeyNew ("/sw", ELEKTRA_KEY_END);
	ElektraKeyset * swKS = elektraKeysetCut (returned, swKey);
	elektraKeyDel (swKey);
	elektraKeysetRewind (swKS);
	ElektraKey * cur;

	ElektraKeyset * appendedKeys = elektraPluginGetData (handle);
	if (!appendedKeys) appendedKeys = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * profileKeys = elektraKeysetNew (0, ELEKTRA_KS_END);
	while ((cur = elektraKeysetNext (swKS)) != NULL)
	{
		if (!fnmatch (PROFILEPATH, elektraKeyName (cur), FNM_PATHNAME))
		{
			elektraKeysetAppendKey (profileKeys, cur);
		}
	}
	linkProfileKeys (swKS, profileKeys, appendedKeys);
	elektraKeysetDel (profileKeys);
	elektraKeysetDel (appendedKeys);
	elektraKeysetRewind (swKS);
	ElektraKeyset * profileParents = elektraKeysetNew (0, ELEKTRA_KS_END);
	while ((cur = elektraKeysetNext (swKS)) != NULL)
	{
		if (!fnmatch (CURRENTPATH, elektraKeyName (cur), FNM_PATHNAME))
		{
			ElektraKey * profileParent = elektraKeyDup (cur, ELEKTRA_KEY_CP_ALL);
			elektraKeyAddName (profileParent, "..");
			elektraKeysetAppendKey (profileParents, elektraKeyDup (profileParent, ELEKTRA_KEY_CP_ALL));
			elektraKeyDel (profileParent);
		}
	}
	linkDefaultKeys (swKS, profileParents, appendedKeys);
	elektraKeysetDel (profileParents);
	elektraKeysetAppend (returned, swKS);
	elektraKeysetDel (swKS);

	return 1; // success
}

int elektraProfileSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned ELEKTRA_UNUSED, ElektraKey * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	ElektraKeyset * appendedKeys = elektraPluginGetData (handle);
	if (!appendedKeys) return 1;
	elektraKeysetRewind (appendedKeys);
	ElektraKey * cur;
	while ((cur = elektraKeysetNext (appendedKeys)) != NULL)
	{
		elektraKeyDel (elektraKeysetLookup (returned, cur, ELEKTRA_KDB_O_POP));
	}
	elektraKeysetDel (appendedKeys);
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


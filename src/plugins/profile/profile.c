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

#include <elektra/kdb.h>     //actual namespaces
#include <kdbease.h> //elektraKeyGetRelativeName
#include <kdbos.h>   //elektraNamespace
#include <stdio.h>
#include <string.h>

#include <fnmatch.h>

#define PROFILEPATH "*/sw/*/*/#*/profile"
#define CURRENTPATH "*/sw/*/*/#*/current"

int elektraProfileOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	KeySet * appendedKeys = NULL;
	elektraPluginSetData (handle, appendedKeys);
	return 1; // success
}

int elektraProfileClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	KeySet * appendedKeys = elektraPluginGetData (handle);
	if (appendedKeys) ksDel (appendedKeys);
	return 1; // success
}

static Key * keyDupWithNS (const Key * origKey, elektraNamespace ns)
{
	Key * newKey = NULL;
	switch (ns)
	{
	case KEY_NS_SPEC:
		newKey = keyNew ("spec", KEY_END);
		break;
	case KEY_NS_DIR:
		newKey = keyNew ("dir", KEY_END);
		break;
	case KEY_NS_USER:
		newKey = keyNew ("user", KEY_END);
		break;
	case KEY_NS_SYSTEM:
		newKey = keyNew ("system", KEY_END);
		break;
	case KEY_NS_PROC:
		newKey = keyNew ("proc", KEY_END);
		break;
	case KEY_NS_CASCADING:
	default:
		newKey = keyNew ("/", KEY_CASCADING_NAME, KEY_END);
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

static void linkProfileKeys (KeySet * swKS, KeySet * profileKeys, KeySet * appendedKeys)
{
	Key * profileCutKey = NULL;
	Key * profileKey = NULL;
	const char * profileString = NULL;
	ksRewind (profileKeys);
	while ((profileKey = ksNext (profileKeys)) != NULL)
	{
		profileString = keyString (profileKey);
		if (profileString)
		{
			profileCutKey = keyDup (profileKey);
			keyAddName (profileCutKey, "..");
			Key * currentProfileKey = keyDup (profileCutKey);
			keyAddBaseName (currentProfileKey, "current");
			keyAddBaseName (profileCutKey, profileString);
			KeySet * profileKS = ksCut (swKS, profileCutKey);
			ksRewind (profileKS);
			Key * cur;
			while ((cur = ksNext (profileKS)) != NULL)
			{
				if (!strcmp (keyName (cur), keyName (profileCutKey))) continue;
				Key * overrideKey = keyDupWithNS (currentProfileKey, KEY_NS_SPEC);
				const char * relativeName = elektraKeyGetRelativeName (cur, profileCutKey);
				keyAddName (overrideKey, relativeName);
				Key * lookupKey = keyDupWithNS (overrideKey, keyGetNamespace (currentProfileKey));
				if (ksLookup (swKS, lookupKey, KDB_O_NONE))
				{
					keyDel (lookupKey);
					keyDel (overrideKey);
					continue;
				}
				keyDel (lookupKey);
				keySetMeta (overrideKey, "override/#0", keyName (cur));
				ksAppendKey (swKS, keyDup (overrideKey));
				ksAppendKey (appendedKeys, keyDup (overrideKey));
				keyDel (overrideKey);
			}
			keyDel (currentProfileKey);
			keyDel (profileCutKey);
			ksAppend (swKS, profileKS);
			ksDel (profileKS);
		}
	}
}

static void linkDefaultKeys (KeySet * swKS, KeySet * profileParents, KeySet * appendedKeys)
{
	ksRewind (profileParents);
	Key * profileParent = NULL;
	while ((profileParent = ksNext (profileParents)) != NULL)
	{
		Key * defaultCutKey = keyDup (profileParent);
		keyAddName (defaultCutKey, "%");
		KeySet * defaultKS = ksCut (swKS, defaultCutKey);
		ksRewind (defaultKS);
		Key * cur;
		Key * currentProfileKey = keyDup (profileParent);
		keyAddName (currentProfileKey, "current");
		while ((cur = ksNext (defaultKS)) != NULL)
		{
			if (!strcmp (keyName (cur), keyName (defaultCutKey))) continue;
			const char * relativeName = elektraKeyGetRelativeName (cur, defaultCutKey);
			Key * overrideKey = keyDupWithNS (currentProfileKey, KEY_NS_SPEC);
			keyAddName (overrideKey, relativeName);
			Key * existingKey = keyDupWithNS (overrideKey, keyGetNamespace (profileParent));
			if (ksLookup (swKS, overrideKey, KDB_O_NONE) || ksLookup (swKS, existingKey, KDB_O_NONE))
			{
				keyDel (overrideKey);
				keyDel (existingKey);
				continue;
			}
			keyDel (existingKey);
			keySetMeta (overrideKey, "override/#0", keyName (cur));
			ksAppendKey (swKS, keyDup (overrideKey));
			ksAppendKey (appendedKeys, keyDup (overrideKey));
			keyDel (overrideKey);
		}
		keyDel (currentProfileKey);
		keyDel (defaultCutKey);
		ksAppend (swKS, defaultKS);
		ksDel (defaultKS);
	}
}

int elektraProfileGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/profile"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system/elektra/modules/profile", KEY_VALUE, "profile plugin waits for your orders", KEY_END),
			       keyNew ("system/elektra/modules/profile/exports", KEY_END),
			       keyNew ("system/elektra/modules/profile/exports/open", KEY_FUNC, elektraProfileOpen, KEY_END),
			       keyNew ("system/elektra/modules/profile/exports/close", KEY_FUNC, elektraProfileClose, KEY_END),
			       keyNew ("system/elektra/modules/profile/exports/get", KEY_FUNC, elektraProfileGet, KEY_END),
			       keyNew ("system/elektra/modules/profile/exports/set", KEY_FUNC, elektraProfileSet, KEY_END),
			       keyNew ("system/elektra/modules/profile/exports/error", KEY_FUNC, elektraProfileError, KEY_END),
#include ELEKTRA_README
			       keyNew ("system/elektra/modules/profile/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	Key * swKey = keyNew ("/sw", KEY_CASCADING_NAME, KEY_END);
	KeySet * swKS = ksCut (returned, swKey);
	keyDel (swKey);
	ksRewind (swKS);
	Key * cur;

	KeySet * appendedKeys = elektraPluginGetData (handle);
	if (!appendedKeys) appendedKeys = ksNew (0, KS_END);
	KeySet * profileKeys = ksNew (0, KS_END);
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
	KeySet * profileParents = ksNew (0, KS_END);
	while ((cur = ksNext (swKS)) != NULL)
	{
		if (!fnmatch (CURRENTPATH, keyName (cur), FNM_PATHNAME))
		{
			Key * profileParent = keyDup (cur);
			keyAddName (profileParent, "..");
			ksAppendKey (profileParents, keyDup (profileParent));
			keyDel (profileParent);
		}
	}
	linkDefaultKeys (swKS, profileParents, appendedKeys);
	ksDel (profileParents);
	ksAppend (returned, swKS);
	ksDel (swKS);

	return 1; // success
}

int elektraProfileSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// get all keys
	KeySet * appendedKeys = elektraPluginGetData (handle);
	if (!appendedKeys) return 1;
	ksRewind (appendedKeys);
	Key * cur;
	while ((cur = ksNext (appendedKeys)) != NULL)
	{
		keyDel (ksLookup (returned, cur, KDB_O_POP));
	}
	ksDel (appendedKeys);
	elektraPluginSetData (handle, NULL);
	return 1; // success
}

int elektraProfileError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
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


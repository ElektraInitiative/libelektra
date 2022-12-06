/**
 * @file
 *
 * @brief Source for profile plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "profile.h"

#include <elektra/kdbhelper.h>

#include <elektra/kdb.h>     //actual namespaces
#include <elektra/kdbease.h> //elektraKeyGetRelativeName
#include <elektra/kdbos.h>   //elektraNamespace
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
		newKey = keyNew ("spec:/", KEY_END);
		break;
	case KEY_NS_DIR:
		newKey = keyNew ("dir:/", KEY_END);
		break;
	case KEY_NS_USER:
		newKey = keyNew ("user:/", KEY_END);
		break;
	case KEY_NS_SYSTEM:
		newKey = keyNew ("system:/", KEY_END);
		break;
	case KEY_NS_PROC:
		newKey = keyNew ("proc:/", KEY_END);
		break;
	case KEY_NS_META:
		newKey = keyNew ("meta:/", KEY_END);
		break;
	case KEY_NS_DEFAULT:
		newKey = keyNew ("default:/", KEY_END);
		break;
	case KEY_NS_CASCADING:
	default:
		newKey = keyNew ("/", KEY_END);
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

	for (elektraCursor it = 0; it < ksGetSize (profileKeys); ++it)
	{
		profileKey = ksAtCursor (profileKeys, it);
		profileString = keyString (profileKey);
		if (profileString)
		{
			profileCutKey = keyDup (profileKey, KEY_CP_ALL);
			keyAddName (profileCutKey, "..");
			Key * currentProfileKey = keyDup (profileCutKey, KEY_CP_ALL);
			keyAddBaseName (currentProfileKey, "current");
			keyAddBaseName (profileCutKey, profileString);
			KeySet * profileKS = ksCut (swKS, profileCutKey);
			Key * cur;

			for (elektraCursor itProfileKS = 0; itProfileKS < ksGetSize (profileKS); ++itProfileKS)
			{
				cur = ksAtCursor (profileKS, itProfileKS);
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
				ksAppendKey (swKS, keyDup (overrideKey, KEY_CP_ALL));
				ksAppendKey (appendedKeys, keyDup (overrideKey, KEY_CP_ALL));
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
	Key * profileParent = NULL;

	for (elektraCursor it = 0; it < ksGetSize (profileParents); ++it)
	{
		profileParent = ksAtCursor (profileParents, it);
		Key * defaultCutKey = keyDup (profileParent, KEY_CP_ALL);
		keyAddName (defaultCutKey, "%");
		KeySet * defaultKS = ksCut (swKS, defaultCutKey);
		Key * cur;
		Key * currentProfileKey = keyDup (profileParent, KEY_CP_ALL);
		keyAddName (currentProfileKey, "current");

		for (elektraCursor itDefaultKS = 0; itDefaultKS < ksGetSize (defaultKS); ++itDefaultKS)
		{
			cur = ksAtCursor (defaultKS, itDefaultKS);
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
			ksAppendKey (swKS, keyDup (overrideKey, KEY_CP_ALL));
			ksAppendKey (appendedKeys, keyDup (overrideKey, KEY_CP_ALL));
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
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/profile"))
	{
		KeySet * contract =
			ksNew (30, keyNew ("system:/elektra/modules/profile", KEY_VALUE, "profile plugin waits for your orders", KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports", KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports/open", KEY_FUNC, elektraProfileOpen, KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports/close", KEY_FUNC, elektraProfileClose, KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports/get", KEY_FUNC, elektraProfileGet, KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports/set", KEY_FUNC, elektraProfileSet, KEY_END),
			       keyNew ("system:/elektra/modules/profile/exports/error", KEY_FUNC, elektraProfileError, KEY_END),
#include ELEKTRA_README
			       keyNew ("system:/elektra/modules/profile/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return 1; // success
	}
	// get all keys
	Key * swKey = keyNew ("/sw", KEY_END);
	KeySet * swKS = ksCut (returned, swKey);
	keyDel (swKey);
	Key * cur;

	KeySet * appendedKeys = elektraPluginGetData (handle);
	if (!appendedKeys) appendedKeys = ksNew (0, KS_END);
	KeySet * profileKeys = ksNew (0, KS_END);

	for (elektraCursor it = 0; it < ksGetSize (swKS); ++it)
	{
		cur = ksAtCursor (swKS, it);
		if (!fnmatch (PROFILEPATH, keyName (cur), FNM_PATHNAME))
		{
			ksAppendKey (profileKeys, cur);
		}
	}
	linkProfileKeys (swKS, profileKeys, appendedKeys);
	ksDel (profileKeys);
	ksDel (appendedKeys);
	KeySet * profileParents = ksNew (0, KS_END);

	for (elektraCursor it = 0; it < ksGetSize (swKS); ++it)
	{
		cur = ksAtCursor (swKS, it);
		if (!fnmatch (CURRENTPATH, keyName (cur), FNM_PATHNAME))
		{
			Key * profileParent = keyDup (cur, KEY_CP_ALL);
			keyAddName (profileParent, "..");
			ksAppendKey (profileParents, keyDup (profileParent, KEY_CP_ALL));
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
	Key * cur;

	for (elektraCursor it = 0; it < ksGetSize (appendedKeys); ++it)
	{
		cur = ksAtCursor (appendedKeys, it);
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


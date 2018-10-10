/**
 * @file
 *
 * @brief Source for reference plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "reference.h"
#include "referencegraph.h"

#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <stdbool.h>

int elektraReferenceGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/reference"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/reference", KEY_VALUE, "reference plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/reference/exports", KEY_END),
			keyNew ("system/elektra/modules/reference/exports/get", KEY_FUNC, elektraReferenceGet, KEY_END),
			keyNew ("system/elektra/modules/reference/exports/set", KEY_FUNC, elektraReferenceSet, KEY_END),
#include ELEKTRA_README (reference)
			keyNew ("system/elektra/modules/reference/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

static bool isReferenceRedundant (const char * reference)
{
	const char * cur = reference;
	while (strncmp (cur, "../", 3) == 0)
	{
		cur += 3;
	}

	return strstr (reference, "/./") != NULL || strstr (cur, "/../") != NULL;
}

static Key * resolveReference (KeySet * allKeys, const char * reference, const char * baseKeyName, Key * parentKey)
{
	if (reference == NULL || strlen (reference) == 0)
	{
		return NULL;
	}

	if (isReferenceRedundant (reference))
	{
		ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_NOCWD, parentKey, "Reference '%s' uses '/./' or '/../' redundantly.", reference);
	}

	Key * fullReference = keyNew ("", KEY_END);

	if (elektraStrNCmp (reference, "@/", 2) == 0)
	{
		keySetName (fullReference, keyName (parentKey));
		keyAddName (fullReference, &reference[2]);
	}
	else if (elektraStrNCmp (reference, "./", 2) == 0)
	{
		keySetName (fullReference, baseKeyName);
		keyAddName (fullReference, &reference[2]);
	}
	else if (elektraStrNCmp (reference, "../", 3) == 0)
	{
		keySetName (fullReference, baseKeyName);
		keyAddName (fullReference, reference);
	}
	else
	{
		keySetName (fullReference, reference);
	}

	return ksLookup (allKeys, fullReference, KDB_O_DEL);
}

static const char * resolveRestriction (const char * restriction, const char * baseKeyName, Key * parentKey)
{
	if (restriction == NULL || strlen (restriction) == 0)
	{
		return NULL;
	}

	if (isReferenceRedundant (restriction))
	{
		ELEKTRA_ADD_WARNINGF (ELEKTRA_WARNING_REFERENCE_REDUNDANT, parentKey, "Restriction '%s' uses '/./' or '/../' redundantly.",
				      restriction);
	}

	Key * fullReference = keyNew ("", KEY_END);

	if (elektraStrNCmp (restriction, "@/", 2) == 0)
	{
		keySetName (fullReference, keyName (parentKey));
		keyAddName (fullReference, &restriction[2]);
	}
	else if (elektraStrNCmp (restriction, "./", 2) == 0)
	{
		keySetName (fullReference, baseKeyName);
		keyAddName (fullReference, &restriction[2]);
	}
	else if (elektraStrNCmp (restriction, "../", 3) == 0)
	{
		keySetName (fullReference, baseKeyName);
		keyAddName (fullReference, restriction);
	}
	else
	{
		keySetName (fullReference, restriction);
	}

	return keyName (fullReference);
}


static bool checkRestriction (const char * name, const char * restriction)
{
	// TODO (kodebach): implement
	return true;
}

static int checkSingleReference (const Key * key, KeySet * allKeys, Key * parentKey)
{
	if (key == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	const char * reference = keyString (key);
	if (reference == NULL || strlen (reference) == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	const Key * restrictKey = keyGetMeta (key, CHECK_REFERENCE_RESTRICT_KEYNAME);
	const char * restrictValue = restrictKey != NULL ? keyString (restrictKey) : NULL;
	const char * restriction = resolveRestriction (restrictValue, keyName (key), parentKey);

	KeySet * refArray;
	if (elektraArrayValidateBaseNameString (reference) >= 0)
	{
		refArray = elektraArrayGet (key, allKeys);
	}
	else
	{
		refArray = ksNew (1, keyDup (key), KS_END);
	}

	ksRewind (refArray);
	const Key * arrayElement;
	while ((arrayElement = ksNext (refArray)) != NULL)
	{
		const char * ref = keyString (arrayElement);
		if (ref == NULL || strlen (ref) == 0)
		{
			continue;
		}

		const char * elementName = keyName (arrayElement);

		Key * refKey = resolveReference (allKeys, ref, elementName, parentKey);
		bool error = false;
		if (refKey == NULL)
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_REFERENCE_NOT_FOUND, parentKey,
					    "Reference '%s', set in key '%s', does not reference an existing key.", ref, elementName);
			error = true;
		}

		if (!checkRestriction (keyName (refKey), restriction))
		{
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_REFERENCE_RESTRICTION, parentKey,
					    "Reference '%s', set in key '%s', does not match restriction '%s'.", ref, elementName,
					    restriction);
			error = true;
		}

		keyDel (refKey);

		if (error)
		{
			ksDel (refArray);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	ksDel (refArray);


	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

static bool checkReferenceGraphAcyclic (const RefGraph * referenceGraph, const char * root)
{
	if (rgEmpty (referenceGraph))
	{
		return true;
	}

	if (!rgHasLeaf (referenceGraph))
	{
		return false;
	}

	RefGraph * curGraph = rgDup (referenceGraph);

	while (rgHasLeaf (curGraph))
	{
		rgRemoveLeaves (curGraph);

		if (rgEmpty (curGraph))
		{
			return true;
		}
	}

	KeySet * nodes = ksNew (0, KS_END);
	ELEKTRA_LOG_NOTICE ("start of path with cycle: %s", root);

	const char * node = root;
	while (ksGetSize (nodes) != ksAppendKey (nodes, keyNew (node, KEY_END)))
	{
		ELEKTRA_LOG_NOTICE ("refers to: %s", node);
		node = rgGetEdge (curGraph, root, 0);
	}

	ELEKTRA_LOG_NOTICE ("already in chain!!");

	return false;
}

static int filterAlternatives (const Key * key, void * argument)
{
	const Key * metaKey = keyGetMeta (key, CHECK_REFERENCE_KEYNAME);
	const char * metaValue = metaKey != NULL ? keyString (metaKey) : NULL;

	const Key * referenceParent = (const Key *) argument;
	return metaValue != NULL && keyIsDirectBelow (referenceParent, key) && strcmp (metaValue, CHECK_REFERNCE_VALUE_ALTERNATIVE) == 0;
}

static int checkRecursiveReference (const Key * rootKey, KeySet * allKeys, Key * parentKey)
{
	if (rootKey == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	RefGraph * referenceGraph = rgNew ();
	KeySet * refnameRoots = ksNew (0, KS_END);

	ksAppendKey (refnameRoots, keyNew (keyName (rootKey), KEY_END));

	Key * curRoot;
	while ((curRoot = ksPop (refnameRoots)) != NULL)
	{
		KeySet * keysToCheck = ksNew (0, KS_END);
		const char * refname = keyBaseName (curRoot);

		Key * rootParent = keyDup (curRoot);
		keySetBaseName (rootParent, NULL);
		ksAppendKey (keysToCheck, rootParent);

		Key * cur;
		while ((cur = ksPop (keysToCheck)) != NULL)
		{
			const char * curName = keyName (cur);
			KeySet * alternatives = ksNew (0, KS_END);
			elektraKsFilter (alternatives, allKeys, filterAlternatives, cur);

			Key * curAlternative;
			while ((curAlternative = ksPop (alternatives)) != NULL)
			{
				ksAppendKey (refnameRoots, keyNew (keyName (curAlternative), KEY_END));
				keyDel (curAlternative);
			}
			ksDel (alternatives);

			Key * tmp = keyNew (curName, KEY_END);
			keyAddBaseName (tmp, refname);
			Key * baseKey = ksLookup (allKeys, tmp, 0);
			keyDel (tmp);

			const char * reference = keyString (baseKey);
			if (reference == NULL || strlen (reference) == 0)
			{
				keyDel (cur);
				keyDel (baseKey);
				continue;
			}

			const Key * restrictKey = keyGetMeta (baseKey, CHECK_REFERENCE_RESTRICT_KEYNAME);
			const char * restrictValue = restrictKey != NULL ? keyString (restrictKey) : NULL;
			const char * restriction = resolveRestriction (restrictValue, keyName (baseKey), parentKey);

			KeySet * refArray;
			if (elektraArrayValidateBaseNameString (reference) >= 0)
			{
				refArray = elektraArrayGet (baseKey, allKeys);
			}
			else
			{
				Key * element = keyDup (baseKey);
				keyAddBaseName (element, "#0");
				refArray = ksNew (1, element, KS_END);
			}
			keyDel (baseKey);

			if (!rgContains (referenceGraph, curName))
			{
				rgAddNode (referenceGraph, curName);
			}

			ksRewind (refArray);
			const Key * arrayElement;
			while ((arrayElement = ksNext (refArray)) != NULL)
			{
				const char * ref = keyString (arrayElement);
				if (ref == NULL || strlen (ref) == 0)
				{
					continue;
				}

				const char * elementName = keyName (arrayElement);

				Key * refKey = resolveReference (allKeys, ref, elementName, parentKey);
				bool error = false;
				if (refKey == NULL)
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_REFERENCE_NOT_FOUND, parentKey,
							    "Reference '%s', set in key '%s', does not reference an existing key.", ref,
							    elementName);
					error = true;
				}

				const char * refKeyName = keyName (refKey);
				if (!checkRestriction (refKeyName, restriction))
				{
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_REFERENCE_RESTRICTION, parentKey,
							    "Reference '%s', set in key '%s', does not match restriction '%s'.", ref,
							    elementName, restriction);
					error = true;
				}

				if (error)
				{
					rgDel (referenceGraph);
					ksDel (keysToCheck);
					ksDel (refnameRoots);
					ksDel (refArray);
					keyDel (curRoot);
					keyDel (refKey);
					keyDel (cur);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (!rgContains (referenceGraph, refKeyName))
				{
					ksAppendKey (keysToCheck, keyDup (refKey));
					rgAddNode (referenceGraph, refKeyName);
				}
				rgAddEdge (referenceGraph, curName, refKeyName);
				keyDel (refKey);
			}
			ksDel (refArray);
			keyDel (cur);
		}
		ksDel (keysToCheck);
		keyDel (curRoot);
	}
	ksDel (refnameRoots);

	char* rootName = elektraStrDup (keyName (rootKey));
	*strrchr (rootName, '/') = '\0';
	if (!checkReferenceGraphAcyclic (referenceGraph, rootName))
	{
		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_REFERENCE_CYCLIC_GRAPH, parentKey, "The configuration contains a cyclic reference.");

		elektraFree (rootName);
		rgDel (referenceGraph);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	elektraFree (rootName);

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraReferenceSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	Key * cur;
	ksRewind (returned);

	int status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	while ((cur = ksNext (returned)) != NULL)
	{
		const Key * metaKey = keyGetMeta (cur, CHECK_REFERENCE_KEYNAME);
		if (metaKey == NULL)
		{
			continue;
		}

		const char * metaValue = keyString (metaKey);

		cursor_t cursor = ksGetCursor (returned);
		if (strcmp (metaValue, CHECK_REFERNCE_VALUE_SINGLE) == 0)
		{
			status |= checkSingleReference (cur, returned, parentKey);
		}

		if (strcmp (metaValue, CHECK_REFERNCE_VALUE_RECURSIVE) == 0)
		{
			status |= checkRecursiveReference (cur, returned, parentKey);
		}
		ksSetCursor (returned, cursor);
	}


	return status;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (reference)
{
	// clang-format off
	return elektraPluginExport ("reference",
		ELEKTRA_PLUGIN_GET,	&elektraReferenceGet,
		ELEKTRA_PLUGIN_SET,	&elektraReferenceSet,
		ELEKTRA_PLUGIN_END);
}

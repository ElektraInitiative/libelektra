/**
 * @file
 *
 * @brief Source for reference plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "reference.h"

#include <kdbease.h>
#include <kdberrors.h>
#include <kdbhelper.h>
#include <stdbool.h>


int elektraReferenceOpen (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// plugin initialization logic
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraReferenceClose (Plugin * handle ELEKTRA_UNUSED, Key * errorKey ELEKTRA_UNUSED)
{
	// free all plugin resources and shut it down
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraReferenceGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system/elektra/modules/reference"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system/elektra/modules/reference", KEY_VALUE, "reference plugin waits for your orders", KEY_END),
			keyNew ("system/elektra/modules/reference/exports", KEY_END),
			keyNew ("system/elektra/modules/reference/exports/open", KEY_FUNC, elektraReferenceOpen, KEY_END),
			keyNew ("system/elektra/modules/reference/exports/close", KEY_FUNC, elektraReferenceClose, KEY_END),
			keyNew ("system/elektra/modules/reference/exports/get", KEY_FUNC, elektraReferenceGet, KEY_END),
			keyNew ("system/elektra/modules/reference/exports/set", KEY_FUNC, elektraReferenceSet, KEY_END),
			keyNew ("system/elektra/modules/reference/exports/error", KEY_FUNC, elektraReferenceError, KEY_END),
			keyNew ("system/elektra/modules/reference/exports/checkconf", KEY_FUNC, elektraReferenceCheckConfig, KEY_END),
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

	return strstr (reference, "/./") == NULL && strstr (cur, "/../") == NULL;
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
		if (!checkRestriction (keyName (refKey), restriction))
		{
			ksDel (refArray);
			keyDel (refKey);
			ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_REFERENCE_RESTRICTION, parentKey,
					    "Reference '%s', set in key '%s', does not match restriction '%s'.", ref, elementName,
					    restriction);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
		keyDel (refKey);
	}
	ksDel (refArray);


	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

static bool isReferenceGraphAcyclic (const KeySet * referenceGraph)
{
	KeySet * curGraph = ksDup (referenceGraph);
	KeySet * nextGraph = ksNew (0, KS_END);

	while (ksGetSize (curGraph) > 0)
	{
		bool leafFound = false;
		Key * cur;
		ksRewind (curGraph);
		while ((cur = ksNext (curGraph)) != NULL)
		{
			if (keyGetMeta (cur, "node") != NULL)
			{
				KeySet * edges = elektraArrayGet (cur, curGraph);
				Key * curEdge;
				bool edgeFound = false;
				while ((curEdge = ksPop (edges)) != NULL)
				{
					Key * refKey = ksLookupByName (curGraph, keyString (curEdge), 0);
					if (refKey != NULL)
					{
						edgeFound = true;
						ksAppendKey (nextGraph, curEdge);
					}
					else
					{
						keyDel (curEdge);
					}
				}
				ksDel (edges);

				if (edgeFound)
				{
					ksAppendKey (nextGraph, keyDup (cur));
				}
				else
				{
					leafFound = true;
				}
			}
			else
			{
				continue;
			}
		}
		ksDel (curGraph);

		if (!leafFound)
		{
			ksDel (nextGraph);
			return false;
		}

		curGraph = nextGraph;
		nextGraph = ksNew (0, KS_END);
	}

	return true;
}

static int filterAlternatives (const Key * key, void * argument)
{
	const Key * metaKey = keyGetMeta (key, CHECK_REFERENCE_KEYNAME);
	const char * metaValue = metaKey != NULL ? keyString (metaKey) : NULL;

	const Key * referenceParent = (const Key *) argument;
	return keyIsDirectBelow (referenceParent, key) && strcmp (metaValue, CHECK_REFERNCE_VALUE_ALTERNATIVE) == 0;
}

static void logCycle (KeySet * cyclicGraph)
{
	ksRewind (cyclicGraph);
	Key * start = ksNext (cyclicGraph);
	while (keyGetMeta (start, "node") == NULL)
	{
		start = ksNext (cyclicGraph);
	}

	const char * startName = keyName (start);
	ELEKTRA_LOG_NOTICE ("start of cycle: %s", startName);

	Key * cur = start;
	for (int i = 0; i < 15; ++i)
	{
		keyAddBaseName (cur, "#0");

		const char * nextName = keyString (cur);
		if (strcmp (nextName, startName) == 0)
		{
			ELEKTRA_LOG_NOTICE ("refers back to start");
			return;
		}

		cur = ksLookupByName (cyclicGraph, nextName, 0);
	}

	ELEKTRA_LOG_NOTICE ("etc. (eventually refers back to start)");
}

static int checkRecursiveReference (const Key * rootKey, KeySet * allKeys, Key * parentKey)
{
	if (rootKey == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	KeySet * referenceGraph = ksNew (0, KS_END);
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
			KeySet * alternatives = ksNew (0, KS_END);
			elektraKsFilter (alternatives, allKeys, filterAlternatives, cur);

			Key * curAlternative;
			while ((curAlternative = ksPop (alternatives)) != NULL)
			{
				ksAppendKey (refnameRoots, keyNew (keyName (curAlternative), KEY_END));
				keyDel (curAlternative);
			}
			ksDel (alternatives);

			keyAddBaseName (cur, refname);

			const char * reference = keyString (cur);
			if (reference == NULL || strlen (reference) == 0)
			{
				keyDel (cur);
				continue;
			}

			const Key * restrictKey = keyGetMeta (cur, CHECK_REFERENCE_RESTRICT_KEYNAME);
			const char * restrictValue = restrictKey != NULL ? keyString (restrictKey) : NULL;
			const char * restriction = resolveRestriction (restrictValue, keyName (cur), parentKey);

			KeySet * refArray;
			if (elektraArrayValidateBaseNameString (reference) >= 0)
			{
				ksAppendKey (referenceGraph, keyNew (keyName (cur), KEY_VALUE, reference, KEY_END));

				refArray = elektraArrayGet (cur, allKeys);
			}
			else
			{
				ksAppendKey (referenceGraph, keyNew (keyName (cur), KEY_VALUE, "#0", KEY_END));

				Key * element = keyDup (cur);
				keyAddBaseName (element, "#0");

				refArray = ksNew (1, element, KS_END);
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
				if (!checkRestriction (keyName (refKey), restriction))
				{
					ksDel (referenceGraph);
					ksDel (keysToCheck);
					ksDel (refnameRoots);
					ksDel (refArray);
					keyDel (cur);
					keyDel (curRoot);
					keyDel (refKey);
					ELEKTRA_SET_ERRORF (ELEKTRA_ERROR_REFERENCE_RESTRICTION, parentKey,
							    "Reference '%s', set in key '%s', does not match restriction '%s'.", ref,
							    elementName, restriction);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}


				ksAppendKey (keysToCheck, keyDup (refKey));
				ksAppendKey (referenceGraph,
					     keyNew (keyName (arrayElement), KEY_VALUE, keyName (refKey), KEY_META, "node", "1", KEY_END));
				keyDel (refKey);
			}
			ksDel (refArray);

			keyDel (cur);
		}
		ksDel (keysToCheck);
		keyDel (curRoot);
	}
	ksDel (refnameRoots);

	if (!isReferenceGraphAcyclic (referenceGraph))
	{

		ELEKTRA_SET_ERROR (ELEKTRA_ERROR_REFERENCE_CYCLIC_GRAPH, parentKey, "The configuration contains a cyclic reference.");

		logCycle (referenceGraph);

		ksDel (referenceGraph);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}

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

		if (strcmp (metaValue, CHECK_REFERNCE_VALUE_SINGLE) == 0)
		{
			status |= checkSingleReference (cur, returned, parentKey);
		}

		if (strcmp (metaValue, CHECK_REFERNCE_VALUE_RECURSIVE) == 0)
		{
			status |= checkRecursiveReference (cur, returned, parentKey);
		}
	}


	return status;
}

int elektraReferenceError (Plugin * handle ELEKTRA_UNUSED, KeySet * returned ELEKTRA_UNUSED, Key * parentKey ELEKTRA_UNUSED)
{
	// handle errors (commit failed)
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_SUCCESS;
}

int elektraReferenceCheckConfig (Key * errorKey ELEKTRA_UNUSED, KeySet * conf ELEKTRA_UNUSED)
{
	// validate plugin configuration
	// this function is optional

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

Plugin * ELEKTRA_PLUGIN_EXPORT (reference)
{
	// clang-format off
	return elektraPluginExport ("reference",
		ELEKTRA_PLUGIN_OPEN,	&elektraReferenceOpen,
		ELEKTRA_PLUGIN_CLOSE,	&elektraReferenceClose,
		ELEKTRA_PLUGIN_GET,	&elektraReferenceGet,
		ELEKTRA_PLUGIN_SET,	&elektraReferenceSet,
		ELEKTRA_PLUGIN_ERROR,	&elektraReferenceError,
		ELEKTRA_PLUGIN_END);
}

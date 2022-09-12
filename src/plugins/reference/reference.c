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
#include <kdbglobbing.h>
#include <kdbhelper.h>
#include <stdbool.h>

int elektraReferenceGet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/reference"))
	{
		ElektraKeyset * contract = ksNew (
			30, keyNew ("system:/elektra/modules/reference", ELEKTRA_KEY_VALUE, "reference plugin waits for your orders", ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/reference/exports", ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/reference/exports/get", ELEKTRA_KEY_FUNC, elektraReferenceGet, ELEKTRA_KEY_END),
			keyNew ("system:/elektra/modules/reference/exports/set", ELEKTRA_KEY_FUNC, elektraReferenceSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/reference/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

static ElektraKey * resolveReference (ElektraKeyset * allKeys, const char * reference, const ElektraKey * baseKey, ElektraKey * parentKey)
{
	if (reference == NULL || strlen (reference) == 0)
	{
		return NULL;
	}

	if (elektraIsReferenceRedundant (reference))
	{
		ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (parentKey, "Reference '%s' uses '/./' or '/../' redundantly", reference);
	}

	char * fullReference = elektraResolveReference (reference, baseKey, parentKey);
	ElektraKey * result = ksLookupByName (allKeys, fullReference, 0);
	elektraFree (fullReference);

	return result;
}

static char * resolveRestriction (const char * restriction, const ElektraKey * baseKey, ElektraKey * parentKey)
{
	if (restriction == NULL || strlen (restriction) == 0)
	{
		return NULL;
	}

	if (elektraIsReferenceRedundant (restriction))
	{
		ELEKTRA_ADD_VALIDATION_SEMANTIC_WARNINGF (parentKey, "Restriction '%s' uses '/./' or '/../' redundantly", restriction);
	}

	return elektraResolveReference (restriction, baseKey, parentKey);
}


static bool checkRestriction (const ElektraKey * key, const char * restriction)
{
	return elektraKeyGlob (key, restriction) == 0;
}

static int checkSingleReference (const ElektraKey * key, ElektraKeyset * allKeys, ElektraKey * parentKey)
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

	const ElektraKey * restrictKey = keyGetMeta (key, CHECK_REFERENCE_RESTRICT_KEYNAME);
	ElektraKeyset * restrictions;
	if (restrictKey != NULL)
	{
		const char * restrictValue = keyString (restrictKey);
		if (elektraArrayValidateBaseNameString (restrictValue) >= 0)
		{
			restrictions = elektraArrayGet (restrictKey, allKeys);
		}
		else
		{
			restrictions = ksNew (1, keyNew ("/#0", ELEKTRA_KEY_VALUE, restrictValue, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		}
	}
	else
	{
		restrictions = ksNew (0, ELEKTRA_KS_END);
	}

	ElektraKeyset * refArray;
	if (elektraArrayValidateBaseNameString (reference) >= 0)
	{
		refArray = elektraArrayGet (key, allKeys);
	}
	else
	{
		refArray = ksNew (1, keyDup (key, ELEKTRA_KEY_CP_ALL), ELEKTRA_KS_END);
	}

	ksRewind (refArray);
	const ElektraKey * arrayElement;
	while ((arrayElement = ksNext (refArray)) != NULL)
	{
		const char * ref = keyString (arrayElement);
		if (ref == NULL || strlen (ref) == 0)
		{
			continue;
		}

		const char * elementName = keyName (arrayElement);

		ElektraKey * refKey = resolveReference (allKeys, ref, arrayElement, parentKey);
		bool error = false;
		if (refKey == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				parentKey, "Reference '%s', set in key '%s', does not reference an existing key", ref, elementName);
			error = true;
		}

		if (ksGetSize (restrictions) > 0)
		{
			ksRewind (restrictions);
			ElektraKey * curRestriction;
			bool anyMatch = false;
			while ((curRestriction = ksNext (restrictions)) != NULL)
			{
				char * restriction = resolveRestriction (keyString (curRestriction), key, parentKey);
				if (checkRestriction (refKey, restriction))
				{
					anyMatch = true;
				}
				elektraFree (restriction);
			}

			if (!anyMatch)
			{
				ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
					parentKey, "Reference '%s', set in key '%s', does not any of the given restrictions", ref,
					elementName);
				error = true;
			}
		}

		if (error)
		{
			ksDel (restrictions);
			ksDel (refArray);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	ksDel (restrictions);
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
			rgDel (curGraph);
			return true;
		}
	}

	ElektraKeyset * nodes = ksNew (0, ELEKTRA_KS_END);
	ELEKTRA_LOG_NOTICE ("start of path with cycle: %s", root);

	const char * node = root;
	while (ksGetSize (nodes) != ksAppendKey (nodes, keyNew (node, ELEKTRA_KEY_END)))
	{
		ELEKTRA_LOG_NOTICE ("refers to: %s", node);
		node = rgGetEdge (curGraph, root, 0);
	}

	ELEKTRA_LOG_NOTICE ("already in chain!!");

	rgDel (curGraph);
	return false;
}

static int filterAlternatives (const ElektraKey * key, void * argument)
{
	const ElektraKey * metaKey = keyGetMeta (key, CHECK_REFERENCE_KEYNAME);
	const char * metaValue = metaKey != NULL ? keyString (metaKey) : NULL;

	const ElektraKey * referenceParent = (const ElektraKey *) argument;
	return metaValue != NULL && keyIsDirectlyBelow (referenceParent, key) && strcmp (metaValue, CHECK_REFERNCE_VALUE_ALTERNATIVE) == 0;
}

static int checkRecursiveReference (const ElektraKey * rootKey, ElektraKeyset * allKeys, ElektraKey * parentKey)
{
	if (rootKey == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	RefGraph * referenceGraph = rgNew ();
	ElektraKeyset * allRefnames = ksNew (0, ELEKTRA_KS_END);
	ElektraKeyset * refnameRoots = ksNew (0, ELEKTRA_KS_END);

	ksAppendKey (refnameRoots, keyNew (keyName (rootKey), ELEKTRA_KEY_END));

	ElektraKey * curRoot;
	while ((curRoot = ksPop (refnameRoots)) != NULL)
	{
		ElektraKeyset * keysToCheck = ksNew (0, ELEKTRA_KS_END);
		const char * refname = keyBaseName (curRoot);

		ElektraKey * rootParent = keyDup (curRoot, ELEKTRA_KEY_CP_ALL);
		keySetBaseName (rootParent, NULL);
		ksAppendKey (keysToCheck, rootParent);

		ElektraKey * cur;
		while ((cur = ksPop (keysToCheck)) != NULL)
		{
			const char * curName = keyName (cur);
			ElektraKeyset * alternatives = ksNew (0, ELEKTRA_KS_END);
			elektraKsFilter (alternatives, allKeys, filterAlternatives, cur);

			ElektraKey * curAlternative;
			while ((curAlternative = ksPop (alternatives)) != NULL)
			{
				if (ksLookup (allRefnames, curAlternative, 0) == NULL)
				{
					ksAppendKey (refnameRoots, keyNew (keyName (curAlternative), ELEKTRA_KEY_END));
					ksAppendKey (allRefnames, keyNew (keyName (curAlternative), ELEKTRA_KEY_END));
				}
				keyDel (curAlternative);
			}
			ksDel (alternatives);

			ElektraKey * tmp = keyNew (curName, ELEKTRA_KEY_END);
			keyAddBaseName (tmp, refname);
			ElektraKey * baseKey = ksLookup (allKeys, tmp, 0);
			keyDel (tmp);

			const char * reference = keyString (baseKey);
			if (reference == NULL || strlen (reference) == 0)
			{
				keyDel (cur);
				continue;
			}

			const ElektraKey * restrictKey = keyGetMeta (baseKey, CHECK_REFERENCE_RESTRICT_KEYNAME);
			ElektraKeyset * restrictions;
			if (restrictKey != NULL)
			{
				const char * restrictValue = keyString (restrictKey);
				if (elektraArrayValidateBaseNameString (restrictValue) >= 0)
				{
					restrictions = elektraArrayGet (restrictKey, allKeys);
				}
				else
				{
					restrictions = ksNew (1, keyNew ("/#0", ELEKTRA_KEY_VALUE, restrictValue, ELEKTRA_KEY_END), ELEKTRA_KS_END);
				}
			}
			else
			{
				restrictions = ksNew (0, ELEKTRA_KS_END);
			}

			ElektraKeyset * refArray;
			if (elektraArrayValidateBaseNameString (reference) >= 0)
			{
				refArray = elektraArrayGet (baseKey, allKeys);
			}
			else
			{
				ElektraKey * element = keyDup (baseKey, ELEKTRA_KEY_CP_ALL);
				keyAddBaseName (element, "#0");
				refArray = ksNew (1, element, ELEKTRA_KS_END);
			}

			if (!rgContains (referenceGraph, curName))
			{
				rgAddNode (referenceGraph, curName);
			}

			ksRewind (refArray);
			const ElektraKey * arrayElement;
			while ((arrayElement = ksNext (refArray)) != NULL)
			{
				const char * ref = keyString (arrayElement);
				if (ref == NULL || strlen (ref) == 0)
				{
					continue;
				}

				const char * elementName = keyName (arrayElement);

				ElektraKey * refKey = resolveReference (allKeys, ref, arrayElement, parentKey);
				bool error = false;
				if (refKey == NULL)
				{
					ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
						parentKey, "Reference '%s', set in key '%s', does not reference an existing key", ref,
						elementName);
					error = true;
				}

				const char * refKeyName = keyName (refKey);

				if (ksGetSize (restrictions) > 0)
				{
					ksRewind (restrictions);
					ElektraKey * curRestriction;
					bool anyMatch = false;
					while ((curRestriction = ksNext (restrictions)) != NULL)
					{
						char * restriction = resolveRestriction (keyString (curRestriction), baseKey, parentKey);
						if (checkRestriction (refKey, restriction))
						{
							anyMatch = true;
						}
						elektraFree (restriction);
					}

					if (!anyMatch)
					{
						ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
							parentKey,
							"Reference '%s', set in key '%s', does not any of the given restrictions", ref,
							elementName);
						error = true;
					}
				}

				if (error)
				{
					rgDel (referenceGraph);
					ksDel (keysToCheck);
					ksDel (refnameRoots);
					ksDel (allRefnames);
					ksDel (refArray);
					ksDel (restrictions);
					keyDel (curRoot);
					keyDel (cur);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (!rgContains (referenceGraph, refKeyName))
				{
					ksAppendKey (keysToCheck, keyDup (refKey, ELEKTRA_KEY_CP_ALL));
					rgAddNode (referenceGraph, refKeyName);
				}
				rgAddEdge (referenceGraph, curName, refKeyName);
			}
			ksDel (refArray);
			ksDel (restrictions);
			keyDel (cur);
		}
		ksDel (keysToCheck);
		keyDel (curRoot);
	}
	ksDel (refnameRoots);
	ksDel (allRefnames);

	char * rootName = elektraStrDup (keyName (rootKey));
	*strrchr (rootName, '/') = '\0';
	if (!checkReferenceGraphAcyclic (referenceGraph, rootName))
	{
		ELEKTRA_SET_VALIDATION_SEMANTIC_ERROR (parentKey, "The configuration contains a cyclic reference");

		elektraFree (rootName);
		rgDel (referenceGraph);
		return ELEKTRA_PLUGIN_STATUS_ERROR;
	}
	rgDel (referenceGraph);
	elektraFree (rootName);

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

int elektraReferenceSet (Plugin * handle ELEKTRA_UNUSED, ElektraKeyset * returned, ElektraKey * parentKey)
{
	ElektraKey * cur;
	ksRewind (returned);

	int status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	while ((cur = ksNext (returned)) != NULL)
	{
		const ElektraKey * metaKey = keyGetMeta (cur, CHECK_REFERENCE_KEYNAME);
		if (metaKey == NULL)
		{
			continue;
		}

		const char * metaValue = keyString (metaKey);

		elektraCursor cursor = ksGetCursor (returned);
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

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("reference",
		ELEKTRA_PLUGIN_GET,	&elektraReferenceGet,
		ELEKTRA_PLUGIN_SET,	&elektraReferenceSet,
		ELEKTRA_PLUGIN_END);
}

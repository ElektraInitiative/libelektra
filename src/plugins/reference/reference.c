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
	if (!elektraStrCmp (elektraKeyName (parentKey), "system:/elektra/modules/reference"))
	{
		ElektraKeyset * contract = elektraKeysetNew (
			30, elektraKeyNew ("system:/elektra/modules/reference", ELEKTRA_KEY_VALUE, "reference plugin waits for your orders", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/reference/exports", ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/reference/exports/get", ELEKTRA_KEY_FUNC, elektraReferenceGet, ELEKTRA_KEY_END),
			elektraKeyNew ("system:/elektra/modules/reference/exports/set", ELEKTRA_KEY_FUNC, elektraReferenceSet, ELEKTRA_KEY_END),
#include ELEKTRA_README
			elektraKeyNew ("system:/elektra/modules/reference/infos/version", ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		elektraKeysetAppend (returned, contract);
		elektraKeysetDel (contract);

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
	ElektraKey * result = elektraKeysetLookupByName (allKeys, fullReference, 0);
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

	const char * reference = elektraKeyString (key);
	if (reference == NULL || strlen (reference) == 0)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	const ElektraKey * restrictKey = elektraKeyGetMeta (key, CHECK_REFERENCE_RESTRICT_KEYNAME);
	ElektraKeyset * restrictions;
	if (restrictKey != NULL)
	{
		const char * restrictValue = elektraKeyString (restrictKey);
		if (elektraArrayValidateBaseNameString (restrictValue) >= 0)
		{
			restrictions = elektraArrayGet (restrictKey, allKeys);
		}
		else
		{
			restrictions = elektraKeysetNew (1, elektraKeyNew ("/#0", ELEKTRA_KEY_VALUE, restrictValue, ELEKTRA_KEY_END), ELEKTRA_KS_END);
		}
	}
	else
	{
		restrictions = elektraKeysetNew (0, ELEKTRA_KS_END);
	}

	ElektraKeyset * refArray;
	if (elektraArrayValidateBaseNameString (reference) >= 0)
	{
		refArray = elektraArrayGet (key, allKeys);
	}
	else
	{
		refArray = elektraKeysetNew (1, elektraKeyDup (key, ELEKTRA_KEY_CP_ALL), ELEKTRA_KS_END);
	}

	elektraKeysetRewind (refArray);
	const ElektraKey * arrayElement;
	while ((arrayElement = elektraKeysetNext (refArray)) != NULL)
	{
		const char * ref = elektraKeyString (arrayElement);
		if (ref == NULL || strlen (ref) == 0)
		{
			continue;
		}

		const char * elementName = elektraKeyName (arrayElement);

		ElektraKey * refKey = resolveReference (allKeys, ref, arrayElement, parentKey);
		bool error = false;
		if (refKey == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				parentKey, "Reference '%s', set in key '%s', does not reference an existing key", ref, elementName);
			error = true;
		}

		if (elektraKeysetGetSize (restrictions) > 0)
		{
			elektraKeysetRewind (restrictions);
			ElektraKey * curRestriction;
			bool anyMatch = false;
			while ((curRestriction = elektraKeysetNext (restrictions)) != NULL)
			{
				char * restriction = resolveRestriction (elektraKeyString (curRestriction), key, parentKey);
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
			elektraKeysetDel (restrictions);
			elektraKeysetDel (refArray);
			return ELEKTRA_PLUGIN_STATUS_ERROR;
		}
	}
	elektraKeysetDel (restrictions);
	elektraKeysetDel (refArray);


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

	ElektraKeyset * nodes = elektraKeysetNew (0, ELEKTRA_KS_END);
	ELEKTRA_LOG_NOTICE ("start of path with cycle: %s", root);

	const char * node = root;
	while (elektraKeysetGetSize (nodes) != elektraKeysetAppendKey (nodes, elektraKeyNew (node, ELEKTRA_KEY_END)))
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
	const ElektraKey * metaKey = elektraKeyGetMeta (key, CHECK_REFERENCE_KEYNAME);
	const char * metaValue = metaKey != NULL ? elektraKeyString (metaKey) : NULL;

	const ElektraKey * referenceParent = (const ElektraKey *) argument;
	return metaValue != NULL && elektraKeyIsDirectlyBelow (referenceParent, key) && strcmp (metaValue, CHECK_REFERNCE_VALUE_ALTERNATIVE) == 0;
}

static int checkRecursiveReference (const ElektraKey * rootKey, ElektraKeyset * allKeys, ElektraKey * parentKey)
{
	if (rootKey == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	RefGraph * referenceGraph = rgNew ();
	ElektraKeyset * allRefnames = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * refnameRoots = elektraKeysetNew (0, ELEKTRA_KS_END);

	elektraKeysetAppendKey (refnameRoots, elektraKeyNew (elektraKeyName (rootKey), ELEKTRA_KEY_END));

	ElektraKey * curRoot;
	while ((curRoot = elektraKeysetPop (refnameRoots)) != NULL)
	{
		ElektraKeyset * keysToCheck = elektraKeysetNew (0, ELEKTRA_KS_END);
		const char * refname = elektraKeyBaseName (curRoot);

		ElektraKey * rootParent = elektraKeyDup (curRoot, ELEKTRA_KEY_CP_ALL);
		elektraKeySetBaseName (rootParent, NULL);
		elektraKeysetAppendKey (keysToCheck, rootParent);

		ElektraKey * cur;
		while ((cur = elektraKeysetPop (keysToCheck)) != NULL)
		{
			const char * curName = elektraKeyName (cur);
			ElektraKeyset * alternatives = elektraKeysetNew (0, ELEKTRA_KS_END);
			elektraKsFilter (alternatives, allKeys, filterAlternatives, cur);

			ElektraKey * curAlternative;
			while ((curAlternative = elektraKeysetPop (alternatives)) != NULL)
			{
				if (elektraKeysetLookup (allRefnames, curAlternative, 0) == NULL)
				{
					elektraKeysetAppendKey (refnameRoots, elektraKeyNew (elektraKeyName (curAlternative), ELEKTRA_KEY_END));
					elektraKeysetAppendKey (allRefnames, elektraKeyNew (elektraKeyName (curAlternative), ELEKTRA_KEY_END));
				}
				elektraKeyDel (curAlternative);
			}
			elektraKeysetDel (alternatives);

			ElektraKey * tmp = elektraKeyNew (curName, ELEKTRA_KEY_END);
			elektraKeyAddBaseName (tmp, refname);
			ElektraKey * baseKey = elektraKeysetLookup (allKeys, tmp, 0);
			elektraKeyDel (tmp);

			const char * reference = elektraKeyString (baseKey);
			if (reference == NULL || strlen (reference) == 0)
			{
				elektraKeyDel (cur);
				continue;
			}

			const ElektraKey * restrictKey = elektraKeyGetMeta (baseKey, CHECK_REFERENCE_RESTRICT_KEYNAME);
			ElektraKeyset * restrictions;
			if (restrictKey != NULL)
			{
				const char * restrictValue = elektraKeyString (restrictKey);
				if (elektraArrayValidateBaseNameString (restrictValue) >= 0)
				{
					restrictions = elektraArrayGet (restrictKey, allKeys);
				}
				else
				{
					restrictions = elektraKeysetNew (1, elektraKeyNew ("/#0", ELEKTRA_KEY_VALUE, restrictValue, ELEKTRA_KEY_END), ELEKTRA_KS_END);
				}
			}
			else
			{
				restrictions = elektraKeysetNew (0, ELEKTRA_KS_END);
			}

			ElektraKeyset * refArray;
			if (elektraArrayValidateBaseNameString (reference) >= 0)
			{
				refArray = elektraArrayGet (baseKey, allKeys);
			}
			else
			{
				ElektraKey * element = elektraKeyDup (baseKey, ELEKTRA_KEY_CP_ALL);
				elektraKeyAddBaseName (element, "#0");
				refArray = elektraKeysetNew (1, element, ELEKTRA_KS_END);
			}

			if (!rgContains (referenceGraph, curName))
			{
				rgAddNode (referenceGraph, curName);
			}

			elektraKeysetRewind (refArray);
			const ElektraKey * arrayElement;
			while ((arrayElement = elektraKeysetNext (refArray)) != NULL)
			{
				const char * ref = elektraKeyString (arrayElement);
				if (ref == NULL || strlen (ref) == 0)
				{
					continue;
				}

				const char * elementName = elektraKeyName (arrayElement);

				ElektraKey * refKey = resolveReference (allKeys, ref, arrayElement, parentKey);
				bool error = false;
				if (refKey == NULL)
				{
					ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
						parentKey, "Reference '%s', set in key '%s', does not reference an existing key", ref,
						elementName);
					error = true;
				}

				const char * refKeyName = elektraKeyName (refKey);

				if (elektraKeysetGetSize (restrictions) > 0)
				{
					elektraKeysetRewind (restrictions);
					ElektraKey * curRestriction;
					bool anyMatch = false;
					while ((curRestriction = elektraKeysetNext (restrictions)) != NULL)
					{
						char * restriction = resolveRestriction (elektraKeyString (curRestriction), baseKey, parentKey);
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
					elektraKeysetDel (keysToCheck);
					elektraKeysetDel (refnameRoots);
					elektraKeysetDel (allRefnames);
					elektraKeysetDel (refArray);
					elektraKeysetDel (restrictions);
					elektraKeyDel (curRoot);
					elektraKeyDel (cur);
					return ELEKTRA_PLUGIN_STATUS_ERROR;
				}

				if (!rgContains (referenceGraph, refKeyName))
				{
					elektraKeysetAppendKey (keysToCheck, elektraKeyDup (refKey, ELEKTRA_KEY_CP_ALL));
					rgAddNode (referenceGraph, refKeyName);
				}
				rgAddEdge (referenceGraph, curName, refKeyName);
			}
			elektraKeysetDel (refArray);
			elektraKeysetDel (restrictions);
			elektraKeyDel (cur);
		}
		elektraKeysetDel (keysToCheck);
		elektraKeyDel (curRoot);
	}
	elektraKeysetDel (refnameRoots);
	elektraKeysetDel (allRefnames);

	char * rootName = elektraStrDup (elektraKeyName (rootKey));
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
	elektraKeysetRewind (returned);

	int status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	while ((cur = elektraKeysetNext (returned)) != NULL)
	{
		const ElektraKey * metaKey = elektraKeyGetMeta (cur, CHECK_REFERENCE_KEYNAME);
		if (metaKey == NULL)
		{
			continue;
		}

		const char * metaValue = elektraKeyString (metaKey);

		elektraCursor cursor = elektraKeysetGetCursor (returned);
		if (strcmp (metaValue, CHECK_REFERNCE_VALUE_SINGLE) == 0)
		{
			status |= checkSingleReference (cur, returned, parentKey);
		}

		if (strcmp (metaValue, CHECK_REFERNCE_VALUE_RECURSIVE) == 0)
		{
			status |= checkRecursiveReference (cur, returned, parentKey);
		}
		elektraKeysetSetCursor (returned, cursor);
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

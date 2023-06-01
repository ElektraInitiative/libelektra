/**
 * @file
 *
 * @brief Source for reference plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "./reference.h"
#include "./referencegraph.h"

#include <elektra/core/errors.h>
#include <elektra/ease/array.h>
#include <elektra/ease/globbing.h>
#include <elektra/ease/reference.h>
#include <elektra/ease/utils.h>
#include <internal/macros/attributes.h>
#include <internal/utility/old_helper.h>
#include <stdbool.h>

int elektraReferenceGet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	if (!elektraStrCmp (keyName (parentKey), "system:/elektra/modules/reference"))
	{
		KeySet * contract = ksNew (
			30, keyNew ("system:/elektra/modules/reference", KEY_VALUE, "reference plugin waits for your orders", KEY_END),
			keyNew ("system:/elektra/modules/reference/exports", KEY_END),
			keyNew ("system:/elektra/modules/reference/exports/get", KEY_FUNC, elektraReferenceGet, KEY_END),
			keyNew ("system:/elektra/modules/reference/exports/set", KEY_FUNC, elektraReferenceSet, KEY_END),
#include ELEKTRA_README
			keyNew ("system:/elektra/modules/reference/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END), KS_END);
		ksAppend (returned, contract);
		ksDel (contract);

		return ELEKTRA_PLUGIN_STATUS_SUCCESS;
	}
	// get all keys

	return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
}

static Key * resolveReference (KeySet * allKeys, const char * reference, const Key * baseKey, Key * parentKey)
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
	Key * result = ksLookupByName (allKeys, fullReference, 0);
	elektraFree (fullReference);

	return result;
}

static char * resolveRestriction (const char * restriction, const Key * baseKey, Key * parentKey)
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


static bool checkRestriction (const Key * key, const char * restriction)
{
	return elektraKeyGlob (key, restriction) == 0;
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
	KeySet * restrictions;
	if (restrictKey != NULL)
	{
		const char * restrictValue = keyString (restrictKey);
		if (elektraArrayValidateBaseNameString (restrictValue) >= 0)
		{
			restrictions = elektraArrayGet (restrictKey, allKeys);
		}
		else
		{
			restrictions = ksNew (1, keyNew ("/#0", KEY_VALUE, restrictValue, KEY_END), KS_END);
		}
	}
	else
	{
		restrictions = ksNew (0, KS_END);
	}

	KeySet * refArray;
	if (elektraArrayValidateBaseNameString (reference) >= 0)
	{
		refArray = elektraArrayGet (key, allKeys);
	}
	else
	{
		refArray = ksNew (1, keyDup (key, KEY_CP_ALL), KS_END);
	}

	for (elektraCursor it = 0; it < ksGetSize (refArray); ++it)
	{
		const Key * arrayElement = ksAtCursor (refArray, it);
		const char * ref = keyString (arrayElement);
		if (ref == NULL || strlen (ref) == 0)
		{
			continue;
		}

		const char * elementName = keyName (arrayElement);

		Key * refKey = resolveReference (allKeys, ref, arrayElement, parentKey);
		bool error = false;
		if (refKey == NULL)
		{
			ELEKTRA_SET_VALIDATION_SEMANTIC_ERRORF (
				parentKey, "Reference '%s', set in key '%s', does not reference an existing key", ref, elementName);
			error = true;
		}

		if (ksGetSize (restrictions) > 0)
		{
			bool anyMatch = false;
			for (elektraCursor itRestrictions = 0; itRestrictions < ksGetSize (restrictions); ++itRestrictions)
			{
				Key * curRestriction = ksAtCursor (restrictions, itRestrictions);
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

	KeySet * nodes = ksNew (0, KS_END);
	ELEKTRA_LOG_NOTICE ("start of path with cycle: %s", root);

	const char * node = root;
	while (ksGetSize (nodes) != ksAppendKey (nodes, keyNew (node, KEY_END)))
	{
		ELEKTRA_LOG_NOTICE ("refers to: %s", node);
		node = rgGetEdge (curGraph, root, 0);
	}

	ELEKTRA_LOG_NOTICE ("already in chain!!");

	rgDel (curGraph);
	return false;
}

static int filterAlternatives (const Key * key, void * argument)
{
	const Key * metaKey = keyGetMeta (key, CHECK_REFERENCE_KEYNAME);
	const char * metaValue = metaKey != NULL ? keyString (metaKey) : NULL;

	const Key * referenceParent = (const Key *) argument;
	return metaValue != NULL && keyIsDirectlyBelow (referenceParent, key) && strcmp (metaValue, CHECK_REFERNCE_VALUE_ALTERNATIVE) == 0;
}

static int checkRecursiveReference (const Key * rootKey, KeySet * allKeys, Key * parentKey)
{
	if (rootKey == NULL)
	{
		return ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	}

	RefGraph * referenceGraph = rgNew ();
	KeySet * allRefnames = ksNew (0, KS_END);
	KeySet * refnameRoots = ksNew (0, KS_END);

	ksAppendKey (refnameRoots, keyNew (keyName (rootKey), KEY_END));

	Key * curRoot;
	while ((curRoot = ksPop (refnameRoots)) != NULL)
	{
		KeySet * keysToCheck = ksNew (0, KS_END);
		const char * refname = keyBaseName (curRoot);

		Key * rootParent = keyDup (curRoot, KEY_CP_ALL);
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
				if (ksLookup (allRefnames, curAlternative, 0) == NULL)
				{
					ksAppendKey (refnameRoots, keyNew (keyName (curAlternative), KEY_END));
					ksAppendKey (allRefnames, keyNew (keyName (curAlternative), KEY_END));
				}
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
				continue;
			}

			const Key * restrictKey = keyGetMeta (baseKey, CHECK_REFERENCE_RESTRICT_KEYNAME);
			KeySet * restrictions;
			if (restrictKey != NULL)
			{
				const char * restrictValue = keyString (restrictKey);
				if (elektraArrayValidateBaseNameString (restrictValue) >= 0)
				{
					restrictions = elektraArrayGet (restrictKey, allKeys);
				}
				else
				{
					restrictions = ksNew (1, keyNew ("/#0", KEY_VALUE, restrictValue, KEY_END), KS_END);
				}
			}
			else
			{
				restrictions = ksNew (0, KS_END);
			}

			KeySet * refArray;
			if (elektraArrayValidateBaseNameString (reference) >= 0)
			{
				refArray = elektraArrayGet (baseKey, allKeys);
			}
			else
			{
				Key * element = keyDup (baseKey, KEY_CP_ALL);
				keyAddBaseName (element, "#0");
				refArray = ksNew (1, element, KS_END);
			}

			if (!rgContains (referenceGraph, curName))
			{
				rgAddNode (referenceGraph, curName);
			}

			for (elektraCursor it = 0; it < ksGetSize (refArray); ++it)
			{
				const Key * arrayElement = ksAtCursor (refArray, it);
				const char * ref = keyString (arrayElement);
				if (ref == NULL || strlen (ref) == 0)
				{
					continue;
				}

				const char * elementName = keyName (arrayElement);

				Key * refKey = resolveReference (allKeys, ref, arrayElement, parentKey);
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
					bool anyMatch = false;
					for (elektraCursor itRestrictions = 0; itRestrictions < ksGetSize (restrictions); ++itRestrictions)
					{
						Key * curRestriction = ksAtCursor (restrictions, itRestrictions);
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
					ksAppendKey (keysToCheck, keyDup (refKey, KEY_CP_ALL));
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

int elektraReferenceSet (Plugin * handle ELEKTRA_UNUSED, KeySet * returned, Key * parentKey)
{
	int status = ELEKTRA_PLUGIN_STATUS_NO_UPDATE;
	for (elektraCursor it = 0; it < ksGetSize (returned); ++it)
	{
		Key * cur = ksAtCursor (returned, it);
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

Plugin * ELEKTRA_PLUGIN_EXPORT
{
	// clang-format off
	return elektraPluginExport ("reference",
		ELEKTRA_PLUGIN_GET,	&elektraReferenceGet,
		ELEKTRA_PLUGIN_SET,	&elektraReferenceSet,
		ELEKTRA_PLUGIN_END);
}

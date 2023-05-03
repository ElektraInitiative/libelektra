/**
 * @file
 *
 * @brief Internal functions for handling the backends KeySet of a KDB instance.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/core/namespace.h>
#include <internal/kdbprivate.h>
#include <stdlib.h>

Key * backendsFindParent (KeySet * backends, const Key * key)
{
	/* Note on runtime:
	   m = number of parts in key, n = size of backends, k = length of name of key

	   This function has a runtime of O(m) if backends uses the hashmap, and O(m*log(n)) otherwise.
	   The old trie solution had a runtime of O(k).
	   We expect k > m*log(n) in most cases.
	*/

	Key * lookup = keyDup (key, KEY_CP_NAME);
	while (keyGetUnescapedNameSize (lookup) > 3)
	{
		Key * parent = ksLookup (backends, lookup, 0);
		if (parent != NULL)
		{
			keyDel (lookup);
			return parent;
		}
		keySetBaseName (lookup, NULL);
	}

	// lookup root key or fallback to default:/
	Key * parent = ksLookup (backends, lookup, 0);
	keyDel (lookup);
	return parent != NULL ? parent : ksLookupByName (backends, "default:/", 0);
}

KeySet * backendsForParentKey (KeySet * backends, Key * parentKey)
{
	KeySet * selected = ksBelow (backends, parentKey);
	if (keyGetNamespace (parentKey) == KEY_NS_CASCADING)
	{
		for (elektraNamespace ns = KEY_NS_FIRST; ns <= KEY_NS_LAST; ++ns)
		{
			switch (ns)
			{
			case KEY_NS_PROC:
			case KEY_NS_DIR:
			case KEY_NS_USER:
			case KEY_NS_SYSTEM:
			case KEY_NS_SPEC:
			case KEY_NS_DEFAULT:
				keySetNamespace (parentKey, ns);
				ksAppendKey (selected, backendsFindParent (backends, parentKey));
				break;
			case KEY_NS_META:
			case KEY_NS_NONE:
			case KEY_NS_CASCADING:
				break;
			}
		}
		keySetNamespace (parentKey, KEY_NS_CASCADING);
	}
	else
	{
		ksAppendKey (selected, backendsFindParent (backends, parentKey));
	}
	ksAppendKey (selected, ksLookupByName (backends, "default:/", 0));
	return selected;
}

static elektraCursor backendsDivideInternal (KeySet * backends, elektraCursor * curBackend, const KeySet * ks, elektraCursor cur)
{
	Key * defaultBackendKey = ksLookupByName (backends, "default:/", 0);
	if (defaultBackendKey == NULL && *curBackend < 0)
	{
		// happens during bootstrap
		*curBackend = 0;
	}

	const BackendData * defaultBackendData = keyValue (defaultBackendKey);
	Key * backendKey = *curBackend < 0 ? defaultBackendKey : ksAtCursor (backends, *curBackend);
	BackendData * backendData = (BackendData *) keyValue (backendKey);

	while (cur < ksGetSize (ks))
	{
		Key * k = ksAtCursor (ks, cur);
		Key * nextBackendKey = *curBackend >= ksGetSize (backends) - 1 ? defaultBackendKey : ksAtCursor (backends, *curBackend + 1);

		if (keyIsBelowOrSame (defaultBackendKey, k) == 1)
		{
			Key * d = keyDup (k, KEY_CP_ALL);
			ksAppendKey (defaultBackendData->keys, d);
		}
		// nextBackendKey == NULL happens during bootstrap
		else if (nextBackendKey != NULL && keyCmp (k, nextBackendKey) >= 0)
		{
			++*curBackend;
			cur = backendsDivideInternal (backends, curBackend, ks, cur);
			continue;
		}
		else if (*curBackend < 0 || keyIsBelowOrSame (backendKey, k) == 1)
		{
			Key * d = keyDup (k, KEY_CP_ALL);
			ksAppendKey (backendData->keys, d);
		}
		else
		{
			break;
		}

		cur++;
	}

	return cur;
}

bool backendsDivide (KeySet * backends, const KeySet * ks)
{
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		BackendData * backendData = (BackendData *) keyValue (ksAtCursor (backends, i));
		ksClear (backendData->keys);
	}

	elektraCursor curBackend = -1;
	elektraCursor ret = backendsDivideInternal (backends, &curBackend, ks, 0);
	return ret == ksGetSize (ks);
}

void backendsMerge (KeySet * backends, KeySet * ks)
{
	for (elektraCursor i = 0; i < ksGetSize (backends); i++)
	{
		const Key * backendKey = ksAtCursor (backends, i);
		BackendData * backendData = (BackendData *) keyValue (backendKey);

		if (keyGetNamespace (backendKey) != KEY_NS_DEFAULT)
		{
			ssize_t size = ksGetSize (backendData->keys);
			backendData->getSize = size;
			ksAppend (ks, backendData->keys);
		}
	}
}

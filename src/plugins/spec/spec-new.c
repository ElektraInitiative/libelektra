#include "spec-new.h"
#include "kdberrors.h"

#include <kdbhelper.h>

static KeySet * extractSpecKeys (KeySet * ks)
{
	KeySet * specKeys = ksNew (0, KS_END);
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);
		if (keyGetNamespace (current) == KEY_NS_SPEC)
		{
			ksAppendKey (specKeys, current);
		}
	}

	Key * specKey = keyNew ("spec:/", KEY_END);
	ksDel (ksCut (ks, specKey));
	keyDel (specKey);

	return specKeys;
}

static int copyMeta (Key * key, Key * specKey)
{
	KeySet * metaKeys = ksDup (keyMeta (specKey));

	for (elektraCursor it = 0; it < ksGetSize (metaKeys); it++)
	{
		Key * current = ksAtCursor (metaKeys, it);

		if (!keyCopyMeta (key, specKey, keyName (current)))
		{
			return -1;
		}
	}

	ksDel (metaKeys);

	return 0;
}

static int copyMetaData (Key * specKey, KeySet * ks)
{
	for (elektraCursor it = 0; it < ksGetSize (ks); it++)
	{
		Key * current = ksAtCursor (ks, it);

		if (copyMeta (current, specKey) != 0)
		{
			ELEKTRA_SET_INTERNAL_ERROR (keyNew ("error/key", KEY_END), "Could not copy ");
			return -1;
		}
	}

	return 0;
}

int elektraSpecCopy (ELEKTRA_UNUSED Plugin * handle, KeySet * returned, ELEKTRA_UNUSED Key * parentKey, ELEKTRA_UNUSED bool isKdbGet)
{
	KeySet * specKeys = extractSpecKeys (returned);

 	for (elektraCursor it = 0; it < ksGetSize (specKeys); it++)
	{
		Key * current = ksAtCursor (specKeys, it);
		if (copyMetaData (current, returned) != 0)
		{
			return ELEKTRA_PLUGIN_ERROR;
		}
	}

	ksAppend (returned, specKeys);

	ksDel (specKeys);

	return 1;
}

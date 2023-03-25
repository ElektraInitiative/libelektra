#include "spec-new.h"

#include "kdb.h"

#include <stdio.h>

#ifndef PARENT_KEY
#define PARENT_KEY "/sw/org"
#endif


int main(void)
{
	KeySet * ks = ksNew (10, keyNew ("spec:/" PARENT_KEY "/a", KEY_META, "othermeta", "hiiiii", KEY_END),
			     keyNew ("user:/" PARENT_KEY "/a", KEY_VALUE, "19", KEY_END), KS_END);

	bool isKdbGet = true;
	Key * parentKey = keyNew (PARENT_KEY, KEY_END);

	int result = elektraSpecCopy (NULL, ks, parentKey, isKdbGet);

	if (result != ELEKTRA_PLUGIN_STATUS_SUCCESS)
	{
		printf ("Failed on copying\n");
		return -1;
	}

	Key * lookup = ksLookupByName (ks, PARENT_KEY "/a", 0);
	if (lookup == 0)
	{
		printf ("No key found, spec copy failed.\n");
		return -1;
	}

	printf ("fetching meta keys...\n");
	KeySet * metaKeysOfLookup = keyMeta (lookup);
	if (metaKeysOfLookup == 0)
	{
		printf ("no meta-keys found\n");
		return -1;
	}

	ssize_t size = ksGetSize (metaKeysOfLookup);
	if (size == -1)
	{
		printf ("can not determine size of meta keys key set\n");
		return -1;
	}

	for (elektraCursor it = 0; it < size; it++)
	{
		Key * metaKey = ksAtCursor (metaKeysOfLookup, it);
		printf ("MetaName: %s\t MetaValue: %s\n", keyName (metaKey), keyString(metaKey));
		keyDel (metaKey);
	}

	keyDel (lookup);
	keyDel (parentKey);
	ksDel (metaKeysOfLookup);

	return 0;
}

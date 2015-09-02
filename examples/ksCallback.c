//![callback]
#include <kdb.h>
#include <stdio.h>
#include <kdbconfig.h>

Key * printTrace (ELEKTRA_UNUSED KeySet *ks, Key *key, ELEKTRA_UNUSED option_t options)
{
	printf("name: %s\n", keyName(key));
	return key;
}

int main()
{
	Key *parentKey = keyNew("/tofind", KEY_END);
	KDB *kdb = kdbOpen(parentKey);
	KeySet *ks = ksNew(20, KS_END);
	Key *search = keyNew("/tofind", KEY_FUNC, printTrace, KEY_END);
	//![basic usage]
	kdbGet(kdb, ks, parentKey);
	ksLookup(ks, search, 0);
	//![basic usage]
	kdbClose(kdb, parentKey);
	ksDel(ks);
	keyDel(search);
	keyDel(parentKey);
}
//![callback]

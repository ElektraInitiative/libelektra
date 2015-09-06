//![callback]
#include <kdb.h>
#include <stdio.h>
#include <kdbconfig.h> // for ELEKTRA_UNUSED
#include <kdbproposal.h> // for KDB_O_CALLBACK

Key * printTrace (ELEKTRA_UNUSED KeySet *ks, Key *key, option_t options)
{
	printf("name: %s, %d\n", keyName(key), options & KDB_O_CALLBACK);
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

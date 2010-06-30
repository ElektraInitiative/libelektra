#include <kdb.h>

int main()
{
	KeySet *myConfig = ksNew(0);
	Key *key = keyNew("system/sw/MyApp",KEY_END);
	KDB *handle = kdbOpen(key);

	kdbGet(handle, myConfig, key);

	keySetName(key, "user/sw/MyApp");
	kdbGet(handle, myConfig, key);

	// check for errors by in key
	keyDel(key);

	key = ksLookupByName(myConfig,"/sw/MyApp/key", 0);
	// check if key is not 0 and work with it...

	ksDel (myConfig); // delete the in-memory configuration


	// maybe you want kdbSet() myConfig here

	kdbClose(handle, 0); // no more affairs with the key database.
}

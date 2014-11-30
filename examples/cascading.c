#include <kdb.h>

#include <stdio.h>

int main()
{
	Key *parentKey = keyNew("", KEY_END);
	KDB *kdb = kdbOpen(parentKey);
	KeySet *ks = ksNew(0, KS_END);
	if (kdbGet(kdb, ks, parentKey) == -1)
	{
		printf("kdbGet empty returned error: %s\n",
			keyString(keyGetMeta(parentKey, "error/number")));
	}
	keyDel(parentKey);
	parentKey = keyNew("meta", KEY_META_NAME, KEY_END);
	if (kdbGet(kdb, ks, parentKey) == -1)
	{
		printf("kdbGet meta returned error: %s\n",
			keyString(keyGetMeta(parentKey, "error/number")));
	}
	keyDel(parentKey);
	parentKey = keyNew("/test/shell/somewhere", KEY_CASCADING_NAME, KEY_END);
	if (kdbGet(kdb, ks, parentKey) == -1)
	{
		printf("kdbGet cascading returned error: %s\n",
			keyString(keyGetMeta(parentKey, "error/number")));
	}
	keyDel(parentKey);

	ksRewind(ks);
	Key *k;
	while ((k = ksNext(ks)))
	{
		printf ("%s = %s\n", keyName(k), keyString(k));
	}

	parentKey = keyNew("", KEY_END);
	if (kdbSet(kdb, ks, parentKey) == -1)
	{
		printf("kdbSet empty returned error: %s\n",
			keyString(keyGetMeta(parentKey, "error/number")));
	}
	keyDel(parentKey);
	parentKey = keyNew("meta", KEY_META_NAME, KEY_END);
	if (kdbSet(kdb, ks, parentKey) == -1)
	{
		printf("kdbSet meta returned error: %s\n",
			keyString(keyGetMeta(parentKey, "error/number")));
	}
	keyDel(parentKey);
	parentKey = keyNew("/test/shell/somewhere", KEY_CASCADING_NAME, KEY_END);
	if (kdbSet(kdb, ks, parentKey) == -1)
	{
		printf("kdbSet cascading returned error: %s\n",
			keyString(keyGetMeta(parentKey, "error/number")));
	}

	ksDel(ks);
	kdbClose(kdb, 0);
	keyDel(parentKey);
}

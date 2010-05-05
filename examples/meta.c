#include <kdb.h>

#include <stdio.h>

int main()
{
	Key *k;
	const Key* meta;
	k = keyNew("user/metakey", KEY_END);
	keySetMeta(k, "hello", "hello_world");

	keySetMeta(k, "mode", "0644");
	keySetMeta(k, "time", "1271234264");
	keySetMeta(k, "empty", "");

	printf ("Metadata hello has the value %s\n", (const char*)keyValue(keyGetMeta(k, "hello")));
	printf ("Metadata mode has the value %s\n", (const char*)keyValue(keyGetMeta(k, "mode")));
	printf ("Metadata time has the value %s\n", (const char*)keyValue(keyGetMeta(k, "time")));
	printf ("Metadata empty has the value %s\n", (const char*)keyValue(keyGetMeta(k, "empty")));

	keySetMeta(k, "hello", "between");

	printf ("Metadata hello now has the value %s\n", (const char*)keyValue(keyGetMeta(k, "hello")));

	keySetMeta(k, "hello", 0);

	printf ("Metadata hello now has the value %s (after dropping)\n", (const char*)keyValue(keyGetMeta(k, "hello")));

	keySetMeta(k, "hello", "goodbye");

	printf ("Metadata hello now has the value %s\n", (const char*)keyValue(keyGetMeta(k, "hello")));

	printf ("Now we will output all meta data of the key:\n");
	keyRewindMeta (k);
	while ((meta = keyNextMeta (k))!=0)
	{
		printf ("%s=%s\n", keyName(meta), (const char*)keyValue(meta));
	}

	keyDel (k);

	return 0;
}

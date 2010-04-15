#include <kdb.h>

#include <stdio.h>

int main()
{
	Key *k;
	const char *name;
	k = keyNew("user/metakey", KEY_END);
	keySetMeta(k, "hello", "hello_world");

	keySetMeta(k, "mode", "0644");
	keySetMeta(k, "time", "1271234264");
	keySetMeta(k, "empty", "");

	printf ("Metadata hello has the value %s\n", keyMeta(k, "hello"));
	printf ("Metadata mode has the value %s\n", keyMeta(k, "mode"));
	printf ("Metadata time has the value %s\n", keyMeta(k, "time"));
	printf ("Metadata empty has the value %s\n", keyMeta(k, "empty"));

	keySetMeta(k, "hello", "between");

	printf ("Metadata hello now has the value %s\n", keyMeta(k, "hello"));

	keySetMeta(k, "hello", 0);

	printf ("Metadata hello now has the value %s (after dropping)\n", keyMeta(k, "hello"));

	keySetMeta(k, "hello", "goodbye");

	printf ("Metadata hello now has the value %s\n", keyMeta(k, "hello"));

	printf ("Now we will output all meta data of the key:\n");
	keyRewindMeta (k);
	while ((name = keyNextMeta (k))!=0)
	{
		printf ("%s=%s\n", name, keyCurrentMeta(k));
	}

	keyDel (k);

	return 0;
}

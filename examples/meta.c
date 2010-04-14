#include <kdb.h>

#include <stdio.h>

int main()
{
	Key *k;
	k = keyNew("user/metakey", KEY_END);
	keySetMeta(k, "hello", "helloworld");

	keySetMeta(k, "mode", "0644");
	keySetMeta(k, "time", "1271234264");

	printf ("Metadata hello has the value %s\n", keyMeta(k, "hello"));
	printf ("Metadata mode has the value %s\n", keyMeta(k, "mode"));
	printf ("Metadata time has the value %s\n", keyMeta(k, "time"));

	keySetMeta(k, "hello", "goodbye");

	printf ("Metadata hello now has the value %s\n", keyMeta(k, "hello"));

	keySetMeta(k, "hello", "baba");
	keySetMeta(k, "something", "else");

	printf ("Metadata hello now has the value %s\n", keyMeta(k, "hello"));

	keySetMeta(k, "hello", "argh");

	printf ("Metadata hello now has the value %s\n", keyMeta(k, "hello"));

	return 0;
}

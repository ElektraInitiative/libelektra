#include <kdb.h>

#include <stdio.h>

int main()
{
	Key *k;
	k = keyNew("user/metakey", KEY_END);
	keySetMeta(k, "metaName", "metaValue");

	printf ("metaName has the value %s\n", keyMeta(k, "metaName"));

	return 0;
}

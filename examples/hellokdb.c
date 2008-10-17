#include <kdb.h>
#include <stdio.h>

int main(void)
{
	Key *k = keyNew ("user/hello", KEY_END);
	KDB *handle = kdbOpen();

	if (kdbGetKey (handle, k) == -1) printf ("could not get key\n");
	else printf ("%s\n", (char*) keyValue(k));
	kdbClose (handle);
	keyDel (k);

	return 0;
}

#include <kdb.h>
#include <stdio.h>

void f(Key *k)
{
	printf ("\tf called with %s\n", keyName(k));
	keySetName (k, "user/delete");
	keyDel (k);
}

void h(Key *k)
{
	printf ("\th called with %s\n", keyName(k));
	keyIncRef (k);

	f(k);

	keyDecRef (k);
}

int main(void)
{
	Key *k = keyNew(0);
	printf ("key has ref %d\n", keyGetRef(k));

	f(k);
	printf ("key is now deleted\n");

	k = keyNew(0);
	keyIncRef (k);
	printf ("key has ref %d\n", keyGetRef(k));

	f(k);
	printf ("key has now name %s\n", keyName(k));

	f(k);

	keyDecRef (k);
	printf ("key has ref %d", keyGetRef(k));
	keyDel (k);
	printf ("key is now deleted\n");

	k = keyNew(0);
	h(k);
	keyDel (k);
	printf ("key is now deleted\n");

	return 0;
}

#include <kdbproposal.h>

#include <stdio.h>

void printNamespace(Key const* k)
{
//! [namespace]
switch (keyGetNamespace(k))
{
case KEY_NS_USER:
	printf ("user namespace\n");
	break;
case KEY_NS_SYSTEM:
	printf ("system namespace\n");
	break;
case KEY_NS_SPEC:
	printf ("spec namespace\n");
	break;
case KEY_NS_EMPTY:
	printf ("empty name\n");
	break;
case KEY_NS_NONE:
	printf ("no key\n");
	break;
case KEY_NS_META:
	printf ("meta key\n");
	break;
case KEY_NS_CASCADING:
	printf ("cascading key\n");
	break;
}
//! [namespace]
}

int main()
{
	char s[100];
	scanf("%99s", &s[0]);

	Key *k = keyNew(s,
		KEY_CASCADING_NAME,
		KEY_META_NAME,
		KEY_END);
	printNamespace(k);
	keyDel(k);
}

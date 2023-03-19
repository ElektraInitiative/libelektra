/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/old_kdb.h>

#include <stdio.h>
#include <stdlib.h>

void printNamespace (Key const * k)
{
	// clang-format off
//! [namespace]
switch (keyGetNamespace (k))
{
case KEY_NS_SPEC:
	printf ("spec namespace\n");
	break;
case KEY_NS_PROC:
	printf ("proc namespace\n");
	break;
case KEY_NS_DIR:
	printf ("dir namespace\n");
	break;
case KEY_NS_USER:
	printf ("user namespace\n");
	break;
case KEY_NS_SYSTEM:
	printf ("system namespace\n");
	break;
case KEY_NS_NONE:
	printf ("no key\n");
	break;
case KEY_NS_META:
	printf ("metakey\n");
	break;
case KEY_NS_CASCADING:
	printf ("cascading key\n");
	break;
}
//! [namespace]
}

void loop (void)
{
//! [loop]
for (elektraNamespace ns = KEY_NS_FIRST; ns <= KEY_NS_LAST; ++ns)
{
	// work with namespace
	printf ("%d\n", ns);
}
//! [loop]
}

int main (void)
{
	char s[100];
	if (!fgets (s, 100, stdin)) {
		fprintf(stderr, "Unable to read from standard input");
		return EXIT_FAILURE;
	}

	Key * k = keyNew (s, KEY_END);
	printNamespace (k);
	keyDel (k);
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <stdio.h>

void f (ElektraKey * k)
{
	printf ("\tf called with %s\n", elektraKeyName (k));
	elektraKeySetName (k, "user:/delete");
	elektraKeyDel (k);
}

void h (ElektraKey * k)
{
	printf ("\th called with %s\n", elektraKeyName (k));
	elektraKeyIncRef (k);

	f (k);

	elektraKeyDecRef (k);
}

int main (void)
{
	ElektraKey * k = elektraKeyNew ("user:/key1", ELEKTRA_KEY_END);
	printf ("key has ref %hu\n", elektraKeyGetRef (k));

	f (k);
	printf ("key is now deleted\n\n");

	k = elektraKeyNew ("user:/key2", ELEKTRA_KEY_END);
	elektraKeyIncRef (k);
	printf ("key has ref %hu\n", elektraKeyGetRef (k));

	f (k);
	printf ("key renamed to %s\n", elektraKeyName (k));

	f (k);

	elektraKeyDecRef (k);
	printf ("key has ref %hu\n", elektraKeyGetRef (k));
	elektraKeyDel (k);
	printf ("key is now deleted\n\n");

	k = elektraKeyNew ("user:/key3", ELEKTRA_KEY_END);
	printf ("key has ref %hu\n", elektraKeyGetRef (k));
	h (k);
	elektraKeyDel (k);
	printf ("key is now deleted\n");

	return 0;
}

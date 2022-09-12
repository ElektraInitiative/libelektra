/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>

#include <stdio.h>

int main (void)
{
	ElektraKey * k;
	ElektraKey * c;
	const ElektraKey * meta;
	k = elektraKeyNew ("user:/metakey", ELEKTRA_KEY_END);
	c = elektraKeyNew ("user:/metacopy", ELEKTRA_KEY_END);

	elektraKeySetMeta (k, "hello", "hello_world");

	elektraKeySetMeta (k, "mode", "0644");
	elektraKeySetMeta (k, "time", "1271234264");
	elektraKeySetMeta (k, "empty", "");

	meta = elektraKeyGetMeta (k, "hello");
	printf ("Metadata %s has the value %s with the value size %zd\n", elektraKeyName (meta), (const char *) elektraKeyValue (meta),
		elektraKeyGetValueSize (meta));
	printf ("Metadata mode has the value %s\n", (const char *) elektraKeyValue (elektraKeyGetMeta (k, "mode")));
	printf ("Metadata time has the value %s\n", (const char *) elektraKeyValue (elektraKeyGetMeta (k, "time")));
	printf ("Metadata empty has the value %s\n", (const char *) elektraKeyValue (elektraKeyGetMeta (k, "empty")));

	if (!elektraKeyGetMeta (k, "nonexist")) printf ("Check if a metadata exist\n");

	elektraKeySetMeta (k, "hello", "between");
	elektraKeyCopyMeta (c, k, "hello");

	if (elektraKeyGetMeta (k, "hello") == elektraKeyGetMeta (c, "hello")) printf ("Check if they point to the same metadata after a copy\n");

	printf ("Metadata hello now has the value %s\n", (const char *) elektraKeyValue (elektraKeyGetMeta (k, "hello")));

	elektraKeySetMeta (k, "hello", 0);

	printf ("Metadata hello now has the value %s (after dropping)\n", (const char *) elektraKeyValue (elektraKeyGetMeta (k, "hello")));

	elektraKeySetMeta (k, "hello", "goodbye");

	printf ("Metadata hello now has the value %s\n", (const char *) elektraKeyValue (elektraKeyGetMeta (k, "hello")));

	printf ("Now we will output all metadata of the key:\n");
	elektraKeyRewindMeta (k);
	while ((meta = elektraKeyNextMeta (k)) != 0)
	{
		printf ("%s=%s\n", elektraKeyName (meta), (const char *) elektraKeyValue (meta));
	}

	elektraKeyDel (k);

	return 0;
}

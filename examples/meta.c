/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>

#include <stdio.h>

int main (void)
{
	Key * k;
	Key * c;
	const Key * meta;
	k = keyNew ("user:/metakey", KEY_END);
	c = keyNew ("user:/metacopy", KEY_END);

	keySetMeta (k, "hello", "hello_world");

	keySetMeta (k, "mode", "0644");
	keySetMeta (k, "time", "1271234264");
	keySetMeta (k, "empty", "");

	meta = keyGetMeta (k, "hello");
	printf ("Metadata %s has the value %s with the value size %zd\n", keyName (meta), (const char *) keyValue (meta),
		keyGetValueSize (meta));
	printf ("Metadata mode has the value %s\n", (const char *) keyValue (keyGetMeta (k, "mode")));
	printf ("Metadata time has the value %s\n", (const char *) keyValue (keyGetMeta (k, "time")));
	printf ("Metadata empty has the value %s\n", (const char *) keyValue (keyGetMeta (k, "empty")));

	if (!keyGetMeta (k, "nonexist")) printf ("Check if a metadata exist\n");

	keySetMeta (k, "hello", "between");
	keyCopyMeta (c, k, "hello");

	if (keyGetMeta (k, "hello") == keyGetMeta (c, "hello")) printf ("Check if they point to the same metadata after a copy\n");

	printf ("Metadata hello now has the value %s\n", (const char *) keyValue (keyGetMeta (k, "hello")));

	keySetMeta (k, "hello", 0);

	printf ("Metadata hello now has the value %s (after dropping)\n", (const char *) keyValue (keyGetMeta (k, "hello")));

	keySetMeta (k, "hello", "goodbye");

	printf ("Metadata hello now has the value %s\n", (const char *) keyValue (keyGetMeta (k, "hello")));

	printf ("Now we will output all metadata of the key:\n");

	KeySet * metaKeys = keyMeta (k);
	for (elektraCursor it = 0; it < ksGetSize (metaKeys); ++it)
	{
		meta = ksAtCursor (metaKeys, it);
		printf ("%s=%s\n", keyName (meta), (const char *) keyValue (meta));
	}
	keyDel (k);

	return 0;
}

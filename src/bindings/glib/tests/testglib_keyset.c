/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/glib/gelektra-keyset.h>
#include <glib-object.h>
#include <tests.h>

static void test_ctor (void)
{
	GElektraKeySet * ks;

	ks = gelektra_keyset_new (0);
	succeed_if (ks != NULL, "unable to create keyset");
	g_object_unref (ks);

	GElektraKey * key = gelektra_key_new ("user:/foo", GELEKTRA_KEY_END);
	ks = gelektra_keyset_new (10, key, GELEKTRA_KEYSET_END);
	succeed_if (ks != NULL, "unable to create keyset");
	g_object_unref (ks);

	KeySet * cks = ksNew (0, KS_END);
	ks = gelektra_keyset_make (cks);
	succeed_if (ks->keyset == cks, "new keyset not wrapped");
	g_object_unref (ks);
}

static void test_basic (void)
{
	GElektraKeySet *ks1, *ks2;
	GElektraKey * key;

	ks1 = gelektra_keyset_new (0);
	succeed_if (gelektra_keyset_len (ks1) == 0, "len must be 0");

	gelektra_keyset_append (ks1, gelektra_key_new ("user:/foo", GELEKTRA_KEY_END));
	succeed_if (gelektra_keyset_len (ks1) == 1, "len must be 1");

	ks2 = gelektra_keyset_dup (ks1);
	succeed_if (gelektra_keyset_len (ks2) == 1, "len must be 1");

	gelektra_keyset_clear (ks2);
	succeed_if (gelektra_keyset_len (ks2) == 0, "len must be 0");

	gelektra_keyset_copy (ks1, ks2);
	succeed_if (gelektra_keyset_len (ks2) == 1, "len must be 1 again");

	gelektra_keyset_clear (ks2);
	gelektra_keyset_append (ks2, gelektra_key_new ("user:/bar", GELEKTRA_KEY_END));
	gelektra_keyset_append_keyset (ks1, ks2);
	succeed_if (gelektra_keyset_len (ks1) == 2, "len must be 2");

	key = gelektra_keyset_pop (ks1);
	succeed_if (gelektra_keyset_len (ks1) == 1, "len must be 1 again");
	g_object_unref (key);

	key = gelektra_key_new ("user:/", GELEKTRA_KEY_END);
	ks2 = gelektra_keyset_cut (ks1, key);
	succeed_if (gelektra_keyset_len (ks2) == 1, "len must be 1");
	g_object_unref (key);
	g_object_unref (ks2);

	g_object_unref (ks1);
}

static void test_searching (void)
{
	GElektraKeySet * ks;
	GElektraKey *key1, *key2;
	const char * name = "user:/bar";

	key1 = gelektra_key_new (name, GELEKTRA_KEY_END);
	g_object_ref (key1);
	ks = gelektra_keyset_new (3, gelektra_key_new ("user:/foo", GELEKTRA_KEY_END), key1,
				  gelektra_key_new ("user:/foobar", GELEKTRA_KEY_END), GELEKTRA_KEYSET_END);

	key2 = gelektra_keyset_lookup (ks, key1, GELEKTRA_KDB_O_NONE);
	succeed_if (gelektra_key_cmp (key1, key2) == 0, "lookup returned different key");
	g_object_unref (key2);

	key2 = gelektra_keyset_lookup_byname (ks, name, GELEKTRA_KDB_O_NONE);
	succeed_if (gelektra_key_cmp (key1, key2) == 0, "lookup returned different key");
	g_object_unref (key2);

	g_object_unref (key1);
	g_object_unref (ks);
}

static void test_iterating (void)
{
	GElektraKeySet * ks;
	GElektraKey *key1, *key2, *tmpkey;

	key1 = gelektra_key_new ("user:/a", GELEKTRA_KEY_END);
	g_object_ref (key1);
	key2 = gelektra_key_new ("user:/c", GELEKTRA_KEY_END);
	g_object_ref (key2);
	ks = gelektra_keyset_new (3, key1, gelektra_key_new ("user:/b", GELEKTRA_KEY_END), key2, GELEKTRA_KEYSET_END);

	gssize pos = 0;
	while ((tmpkey = gelektra_keyset_at (ks, pos)) != NULL)
	{
		++pos;
		g_object_unref (tmpkey);
	}
	succeed_if (pos == 3, "some keys are missing");


	tmpkey = gelektra_keyset_at (ks, gelektra_keyset_len (ks) - 1);
	succeed_if (gelektra_key_cmp (tmpkey, key2) == 0, "keyset_at for last position returned unexpected key");
	g_object_unref (tmpkey);

	tmpkey = gelektra_keyset_at (ks, 0);
	succeed_if (gelektra_key_cmp (tmpkey, key1) == 0, "keyset_at for first position returned unexpected key");
	g_object_unref (tmpkey);

	g_object_unref (key1);
	g_object_unref (key2);
	g_object_unref (ks);
}

int main (int argc, char ** argv)
{
	printf ("KEYSET TESTS\n");
	printf ("===============\n\n");

	init (argc, argv);

	test_ctor ();
	test_basic ();
	test_searching ();
	test_iterating ();

	printf ("\n%s RESULTS: %d test(s) done. %d error(s).\n", argv[0], nbTest, nbError);
	return nbError;
}

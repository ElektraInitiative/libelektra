/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/glib/gelektra-kdb.h>
#include <glib-object.h>
#include <tests.h>

#define TEST_NS "user:/tests/glib"

static void test_open_close (void)
{
	GElektraKdb * kdb;
	GElektraKey * error = gelektra_key_new (NULL);

	/* open */
	kdb = gelektra_kdb_open (NULL, error);
	succeed_if (kdb != NULL, "unable to open kdb");
	succeed_if (!gelektra_key_hasmeta (error, "error"), "unexpected error");

	/* close */
	gelektra_kdb_close (kdb, error);
	succeed_if (!gelektra_key_hasmeta (error, "error"), "unexpected error");
	g_object_unref (kdb);

	/* open + close */
	KDB * ckdb = kdbOpen (NULL, error->key);
	kdb = gelektra_kdb_make (ckdb);
	succeed_if (kdb->handle == ckdb, "handle not wrapped");
	g_object_unref (kdb);

	g_object_unref (error);
}


static void test_get_set (void)
{
	GElektraKdb * kdb;
	GElektraKeySet * ks;
	GElektraKey *key, *base, *error;

	/*** set ***/
	/* open kdb */
	error = gelektra_key_new (NULL);
	kdb = gelektra_kdb_open (NULL, error);

	ks = gelektra_keyset_new (100, GELEKTRA_KEYSET_END);

	/* fetch keys below user:/MyApp */
	base = gelektra_key_new (TEST_NS, GELEKTRA_KEY_END);
	gelektra_kdb_get (kdb, ks, base);

	/* search for user:/MyApp/mykey */
	key = gelektra_keyset_lookup_byname (ks, TEST_NS "/mykey", GELEKTRA_KDB_O_NONE);
	if (!key)
	{
		/* key doesn't exist, create a new one and append to keyset */
		key = gelektra_key_new (TEST_NS "/mykey", GELEKTRA_KEY_END);
		/* make sure key still exists after append */
		g_object_ref (key);
		gelektra_keyset_append (ks, key);
	}

	/* set new value */
	gelektra_key_setstring (key, "new_value");
	g_object_unref (key);

	/* store keyset */
	gelektra_kdb_set (kdb, ks, base);

	/* close kdb, flushing the data to disk */
	gelektra_kdb_close (kdb, NULL);
	g_object_unref (kdb);

	/*** get ***/
	/* open kdb again */
	kdb = gelektra_kdb_open (NULL, error);

	/* check if the value is stored */
	gelektra_keyset_clear (ks);
	gelektra_kdb_get (kdb, ks, base);
	key = gelektra_keyset_lookup_byname (ks, TEST_NS "/mykey", GELEKTRA_KDB_O_NONE);
	succeed_if (key != NULL, "key hasn't been stored");
	g_object_unref (key);

	/* close kdb. if we don't care about the error value unref is enough */
	g_object_unref (kdb);

	/*** cleanup ***/
	kdb = gelektra_kdb_open (NULL, error);
	gelektra_keyset_clear (ks);
	gelektra_kdb_get (kdb, ks, base);
	gelektra_keyset_cut (ks, base);
	gelektra_kdb_set (kdb, ks, base);
	g_object_unref (kdb);

	g_object_unref (base);
	g_object_unref (ks);
	g_object_unref (error);
}

int main (int argc, char ** argv)
{
	printf ("KEYSET TESTS\n");
	printf ("===============\n\n");

	init (argc, argv);

	test_open_close ();
	test_get_set ();

	printf ("\n%s RESULTS: %d test(s) done. %d error(s).\n", argv[0], nbTest, nbError);
	return nbError;
}

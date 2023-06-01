/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/glib/gelektra-key.h>
#include <glib-object.h>
#include <tests.h>

static void test_ctor (void)
{
	GElektraKey * key;

	key = gelektra_key_new (NULL);
	succeed_if (key != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (key), "key should be valid");
	g_object_unref (key);

	key = g_object_new (GELEKTRA_TYPE_KEY, NULL);
	succeed_if (key != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (key), "key should be valid");
	g_object_unref (key);

	key = gelektra_key_new ("wrongname", GELEKTRA_KEY_END);
	succeed_if (key == NULL, "created invalid key");

	Key * ckey = keyNew ("/", KEY_END);
	key = gelektra_key_make (ckey);
	succeed_if (key->key == ckey, "new key not wrapped");
	g_object_unref (key);

	key = gelektra_key_new ("user:/foo", GELEKTRA_KEY_END);
	succeed_if (key != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (key), "key should be valid");
	succeed_if (gelektra_key_getref (key) == 1, "refcount should be 1");
	g_object_unref (key);

	key = gelektra_key_new ("/cascading", GELEKTRA_KEY_END);
	succeed_if (key != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (key), "key should be valid");
	succeed_if (gelektra_key_getref (key) == 1, "refcount should be 1");
	g_object_unref (key);

	key = gelektra_key_new ("spec:/key", GELEKTRA_KEY_END);
	succeed_if (key != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (key), "key should be valid");
	succeed_if (gelektra_key_getref (key) == 1, "refcount should be 1");
	g_object_unref (key);

	key = gelektra_key_new ("proc:/key", GELEKTRA_KEY_END);
	succeed_if (key != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (key), "key should be valid");
	succeed_if (gelektra_key_getref (key) == 1, "refcount should be 1");
	g_object_unref (key);

	key = gelektra_key_new ("dir:/key", GELEKTRA_KEY_END);
	succeed_if (key != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (key), "key should be valid");
	succeed_if (gelektra_key_getref (key) == 1, "refcount should be 1");
	g_object_unref (key);

	key = gelektra_key_new ("user:/key", GELEKTRA_KEY_END);
	succeed_if (key != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (key), "key should be valid");
	succeed_if (gelektra_key_getref (key) == 1, "refcount should be 1");
	g_object_unref (key);

	key = gelektra_key_new ("system:/key", GELEKTRA_KEY_END);
	succeed_if (key != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (key), "key should be valid");
	succeed_if (gelektra_key_getref (key) == 1, "refcount should be 1");
	g_object_unref (key);
}

static GElektraKey * g_key = NULL;
static GElektraKey * g_bkey = NULL;

static void create_global_keys (void)
{
	g_key = gelektra_key_new ("user:/key", GELEKTRA_KEY_VALUE, "value", GELEKTRA_KEY_META, "comment/#0", "mycomment", GELEKTRA_KEY_META,
				  "by", "manuel", GELEKTRA_KEY_END);
	succeed_if (g_key != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (g_key), "key should be valid");
	succeed_if (gelektra_key_getref (g_key) == 1, "refcount should be 1");

	g_bkey = gelektra_key_new ("system:/bkey", GELEKTRA_KEY_BINARY, GELEKTRA_KEY_VALUE, "bvalue\0\0", GELEKTRA_KEY_END);
	succeed_if (g_bkey != NULL, "unable to create key");
	succeed_if (gelektra_key_isvalid (g_bkey), "key should be valid");
	succeed_if (gelektra_key_getref (g_bkey) == 1, "refcount should be 1");
}

static void test_props (void)
{
	gchar *name, *basename;
	g_object_get (g_key, "name", &name, "basename", &basename, NULL);
	succeed_if (!strcmp (name, "user:/key"), "wrong value");
	succeed_if (!strcmp (basename, "key"), "wrong value");
	g_free (name);
	g_free (basename);

	GElektraKey * key = g_object_new (GELEKTRA_TYPE_KEY, NULL);
	gelektra_key_setname (key, "user:/foo");
	gelektra_key_setbasename (key, "bar");
	g_object_get (key, "name", &name, NULL);
	succeed_if (!strcmp (name, "user:/bar"), "wrong value");
	g_free (name);
	g_object_unref (key);
}

static void test_basic (void)
{
	GElektraKey * key;

	key = gelektra_key_dup (g_key, KEY_CP_ALL);
	gelektra_key_incref (key);
	succeed_if (gelektra_key_getref (key) == 2, "refcount should be 2");
	succeed_if (gelektra_key_getref (g_key) == 1, "refcount should be still 1");
	gelektra_key_decref (key);
	succeed_if (gelektra_key_getref (key) == 1, "refcount should be 1 again");
	g_object_unref (key);

	gchar * name;
	key = gelektra_key_new ("user:/bar", GELEKTRA_KEY_END);
	gelektra_key_copy (g_key, key, KEY_CP_ALL);
	g_object_get (key, "name", &name, NULL);
	succeed_if (!strcmp (name, "user:/key"), "wrong value");
	g_free (name);

	gelektra_key_clear (key);
	g_object_get (key, "name", &name, NULL);
	succeed_if (!strcmp (name, "/"), "wrong value");
	g_free (name);

	g_object_unref (key);
}

static void test_operators (void)
{
	succeed_if (!gelektra_key_equal (g_key, g_bkey), "keys shouldn't be equal");
	succeed_if (gelektra_key_cmp (g_key, g_bkey) != 0, "keys shouldn't be equal");

	GElektraKey * key = gelektra_key_dup (g_key, KEY_CP_ALL);
	succeed_if (gelektra_key_equal (g_key, key), "keys should be equal");
	succeed_if (gelektra_key_cmp (g_key, key) == 0, "keys should be equal");
	g_object_unref (key);
}

static void test_name_manipulation (void)
{
	succeed_if (gelektra_key_getnamesize (g_key) == sizeof ("user:/key"), "wrong size");
	succeed_if (gelektra_key_getbasenamesize (g_key) == sizeof ("key"), "wrong size");
}


static void test_value_operations (void)
{
	gchar * data;
	void * bdata;
	gssize size;

	/* string value tests */
	size = gelektra_key_getvaluesize (g_key);
	succeed_if (size > 0, "wrong value size");
	data = g_malloc0 (size);
	gelektra_key_getstring (g_key, data, size);
	succeed_if (!strncmp (data, "value", size), "invalid key value");
	g_free (data);

	succeed_if (!memcmp (gelektra_key_getvalue (g_key), "value", sizeof ("value")), "invalid key value");

	gelektra_key_setstring (g_key, "value2");

	data = gelektra_key_gi_getstring (g_key);
	succeed_if (!strcmp (data, "value2"), "invalid key value");
	g_free (data);

	/* binary value tests */
	size = gelektra_key_getvaluesize (g_bkey);
	bdata = g_malloc0 (size);
	gelektra_key_getbinary (g_bkey, bdata, size);
	succeed_if (!memcmp (bdata, "bvalue\0\0", size), "invalid key value");
	g_free (bdata);

	succeed_if (!memcmp (gelektra_key_getvalue (g_bkey), "bvalue\0\0", size), "invalid key value");

	gelektra_key_setbinary (g_bkey, "bvalue2\0\0", sizeof ("bvalue2\0\0"));

	bdata = gelektra_key_gi_getbinary (g_bkey, &size);
	succeed_if (size == sizeof ("bvalue2\0\0"), "invalid key size");
	succeed_if (!memcmp (bdata, "bvalue2\0\0", size), "invalid key value");
	g_free (bdata);
}

static void test_meta_data (void)
{
	GElektraKey *meta, *key;

	/* get tests */
	succeed_if (gelektra_key_hasmeta (g_key, "by"), "no meta");

	meta = gelektra_key_getmeta (g_key, "by");
	succeed_if (gelektra_key_getref (meta) == 2, "refcount should be 2");
	succeed_if (!memcmp (gelektra_key_getvalue (meta), "manuel", sizeof ("manuel")), "invalid metavalue");
	g_object_unref (meta);

	guint metacnt = 0;

	GElektraKeySet * metaKeys = gelektra_key_meta (g_key);
	for (ssize_t it = 0; it < gelektra_keyset_len (metaKeys); ++it)
	{
		meta = gelektra_keyset_at (metaKeys, it);
		GElektraKey * curmeta = gelektra_key_getmeta (g_key, gelektra_key_name (meta));
		succeed_if (meta->key == curmeta->key, "keyset_at and key_getmeta returned different keys of keyset with metakeys");
		g_object_unref (curmeta);

		++metacnt;
		g_object_unref (meta);
	}
	succeed_if_fmt (metacnt == 2, "incorrect number of metadata: %d", metacnt);

	gelektra_key_setmeta (g_key, "by", "gelektra");
	meta = gelektra_key_getmeta (g_key, "by");
	succeed_if (!memcmp (gelektra_key_getvalue (meta), "gelektra", sizeof ("gelektra")), "invalid metavalue");
	g_object_unref (meta);

	/* set tests */
	key = g_object_new (GELEKTRA_TYPE_KEY, NULL);

	gelektra_key_copymeta (g_key, key, "by");
	meta = gelektra_key_getmeta (g_key, "by");
	succeed_if (!memcmp (gelektra_key_getvalue (meta), "gelektra", sizeof ("gelektra")), "invalid metavalue");
	g_object_unref (meta);

	g_object_unref (key);
}

static void test_validating (void)
{
	succeed_if (!gelektra_key_isnull (g_key), "key is null");
	succeed_if (gelektra_key_isvalid (g_key), "key is not valid");
	succeed_if (gelektra_key_isuser (g_key), "key is not user");
	succeed_if (!gelektra_key_issystem (g_key), "key is system");
	succeed_if (gelektra_key_isstring (g_key), "key is not string");
	succeed_if (!gelektra_key_isbinary (g_key), "key is binary");

	succeed_if (!gelektra_key_isuser (g_bkey), "key is user");
	succeed_if (gelektra_key_issystem (g_bkey), "key is not system");
	succeed_if (!gelektra_key_isstring (g_bkey), "key is string");
	succeed_if (gelektra_key_isbinary (g_bkey), "key is not binary");

	GElektraKey * key = gelektra_key_new ("user:/key/glib/edy", GELEKTRA_KEY_END);
	succeed_if (gelektra_key_isbelow (key, g_key), "key not below g_key");
	succeed_if (gelektra_key_isbeloworsame (key, g_key), "key not below g_key");
	succeed_if (gelektra_key_isbeloworsame (g_key, g_key), "key is not key :)");
	succeed_if (!gelektra_key_isdirectbelow (key, g_key), "key not direct below g_key");
	g_object_unref (key);
}

static void destroy_global_keys (void)
{
	g_object_unref (g_key);
	g_object_unref (g_bkey);
}

int main (int argc, char ** argv)
{
	printf ("KEY TESTS\n");
	printf ("===============\n\n");

	init (argc, argv);

	test_ctor ();

	create_global_keys ();
	test_props ();
	test_basic ();
	test_operators ();
	test_name_manipulation ();
	test_value_operations ();
	test_meta_data ();
	test_validating ();
	destroy_global_keys ();

	printf ("\n%s RESULTS: %d test(s) done. %d error(s).\n", argv[0], nbTest, nbError);
	return nbError;
}

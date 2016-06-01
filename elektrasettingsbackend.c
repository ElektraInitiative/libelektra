#define G_SETTINGS_ENABLE_BACKEND
#include <gio/gio.h>
#include <gio/gsettingsbackend.h>
#include <glib.h>

#include <elektra/gelektra-kdb.h>
#include <elektra/gelektra-key.h>
#include <elektra/gelektra-keyset.h>

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "ElektraSettings"

#define G_ELEKTRA_TEST_STRING "test"

#define G_ELEKTRA_SETTINGS_SYSTEM "system"
#define G_ELEKTRA_SETTINGS_USER "user"
#define G_ELEKTRA_SETTINGS_SW "/sw"

typedef GSettingsBackendClass ElektraSettingsBackendClass;

typedef struct
{
	GSettingsBackend backend;
	/*< private >*/
	GElektraKey * gkey;
	GElektraKdb * gkdb;
	GElektraKeySet * gks;
} ElektraSettingsBackend;

/**
 * SECTION:gsettingsbackend
 * @title: ElektraSettingsBackend
 * @short_description: Implementation of the GSeetingBackend Interface with Elektra
 * @include: gio/gsettingsbackend.h
 * @see_also: #GSettings, #GIOExtensionPoint
 *
 * Description of the GSettingsBackend:
 *
 * The #GSettingsBackend interface defines a generic interface for
 * non-strictly-typed data that is stored in a hierarchy. To implement
 * an alternative storage backend for #GSettings, you need to implement
 * the #GSettingsBackend interface and then make it implement the
 * extension point #G_SETTINGS_BACKEND_EXTENSION_POINT_NAME.
 *
 * The interface defines methods for reading and writing values, a
 * method for determining if writing of certain values will fail
 * (lockdown) and a change notification mechanism.
 *
 * The semantics of the interface are very precisely defined and
 * implementations must carefully adhere to the expectations of
 * callers that are documented on each of the interface methods.
 *
 * Some of the GSettingsBackend functions accept or return a #GTree.
 * These trees always have strings as keys and #GVariant as values.
 * g_settings_backend_create_tree() is a convenience function to create
 * suitable trees.
 *
 * The GSettingsBackend API is exported to allow third-party
 * implementations, but does not carry the same stability guarantees
 * as the public GIO API. For this reason, you have to define the
 * C preprocessor symbol %G_SETTINGS_ENABLE_BACKEND before including
 * `gio/gsettingsbackend.h`.
 **/

static GType elektra_settings_backend_get_type (void);
G_DEFINE_TYPE (ElektraSettingsBackend, elektra_settings_backend, G_TYPE_SETTINGS_BACKEND)

static GVariant * elektra_settings_read_string (GSettingsBackend * backend, gchar * keypathname, const GVariantType * expected_type)
{
	ElektraSettingsBackend * esb = (ElektraSettingsBackend *)backend;
	/* Lookup the requested key */
	GElektraKey * gkey = gelektra_keyset_lookup_byname (esb->gks, keypathname, GELEKTRA_KDB_O_NONE);
	/* free the passed path string */
	g_free (keypathname);
	if (gkey == NULL)
	{
		/* key not found */
		return NULL;
	}
	else
	{
		GVariant * read_gvariant;
		gssize length;
		GError * err = NULL;
		gchar * string_value = g_malloc (gelektra_key_getvaluesize (gkey));
		length = gelektra_key_getstring (gkey, string_value, gelektra_key_getvaluesize (gkey));
		read_gvariant = g_variant_parse (expected_type, string_value, NULL, NULL, &err);
		if (err != NULL)
		{
			g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s", "Error parsing string value: ", err->message);
			g_error_free (err);
			return NULL;
		}
	 	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s", "Value is:", string_value);
		return read_gvariant;
	}
}

static gboolean elektra_settings_write_string (GSettingsBackend * backend, const gchar * key, gchar * keypathname, GVariant * value,
					       gpointer origin_tag)
{
	ElektraSettingsBackend * esb = (ElektraSettingsBackend *)backend;
	GElektraKey * gkey = gelektra_keyset_lookup_byname (esb->gks, keypathname, GELEKTRA_KDB_O_NONE);
	const gchar * string_value = g_variant_print (value, FALSE);
	if (gkey == NULL)
	{
		gkey = gelektra_key_new (keypathname, KEY_VALUE, string_value, KEY_END);
		g_free (keypathname);
		if (gkey == NULL || gelektra_keyset_append (esb->gks, gkey) == -1) return FALSE;
	}
	else
	{
		g_free (keypathname);
		// Should we check if correct value is set?
		gelektra_key_setstring (gkey, string_value);
	}
	g_settings_backend_changed (backend, key, origin_tag);
	return TRUE;
}

/* elektra_settings_backend_read implements g_settings_backend_read:
 * @backend: a #GSettingsBackend implementation
 * @key: the key to read
 * @expected_type: a #GVariantType
 * @default_value: if the default value should be returned
 *
 * Reads a key. This call will never block.
 *
 * If the key exists, the value associated with it will be returned.
 * If the key does not exist, %NULL will be returned.
 *
 * The returned value will be of the type given in @expected_type.  If
 * the backend stored a value of a different type then %NULL will be
 * returned.
 *
 * If @default_value is %TRUE then this gets the default value from the
 * backend (ie: the one that the backend would contain if
 * g_settings_reset() were called).
 *
 * Returns: the value that was read, or %NULL
 */
static GVariant * elektra_settings_backend_read (GSettingsBackend * backend, const gchar * key, const GVariantType * expected_type,
						 gboolean default_value)
{
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s %s %.*s %s %s%s", "function read key:", key, "expected_type is:",
	       (int)(g_variant_type_get_string_length (expected_type) & INT_MAX), g_variant_type_peek_string (expected_type), "and we",
	       (default_value ? "" : "do not "), "want the default_value");
	if (default_value)
	{
		return elektra_settings_read_string (backend, g_strconcat (G_ELEKTRA_SETTINGS_SYSTEM, G_ELEKTRA_SETTINGS_SW, key, NULL),
					      expected_type);
	}
	else
	{
		return elektra_settings_read_string (backend, g_strconcat (G_ELEKTRA_SETTINGS_SW, key, NULL), expected_type);
	}
}

/* elektra_settings_backend_read_user_value implements g_settings_backend_read_user_value:
 * @backend: a #GSettingsBackend implementation
 * @key: the key to read
 * @expected_type: a #GVariantType
 *
 * Reads the 'user value' of a key.
 *
 * This is the value of the key that the user has control over and has
 * set for themselves.  Put another way: if the user did not set the
 * value for themselves, then this will return %NULL (even if the
 * sysadmin has provided a default value).
 *
 * Returns: the value that was read, or %NULL
 */
static GVariant * elektra_settings_backend_read_user_value (GSettingsBackend * backend, const gchar * key,
							    const GVariantType * expected_type)
{
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s\n%s %s", "function read (user) key:", key, "expected_type is:",
	       g_variant_type_peek_string (expected_type));
	return elektra_settings_read_string (backend, g_strconcat (G_ELEKTRA_SETTINGS_USER, G_ELEKTRA_SETTINGS_SW, key, NULL), expected_type);
}

/* elektra_settings_backend_write implements g_settings_backend_write:
 * @backend: a #GSettingsBackend implementation
 * @key: the name of the key
 * @value: a #GVariant value to write to this key
 * @origin_tag: the origin tag
 *
 * Writes exactly one key.
 *
 * This call does not fail.  During this call a
 * #GSettingsBackend::changed signal will be emitted if the value of the
 * key has changed.  The updated key value will be visible to any signal
 * callbacks.
 *
 * One possible method that an implementation might deal with failures is
 * to emit a second "changed" signal (either during this call, or later)
 * to indicate that the affected keys have suddenly "changed back" to their
 * old values.
 *
 * Returns: %TRUE if the write succeeded, %FALSE if the key was not writable
 */
static gboolean elektra_settings_backend_write (GSettingsBackend * backend, const gchar * key, GVariant * value, gpointer origin_tag)
{
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s %s %s", "function write key: ", key, "value is:", g_variant_print (value, TRUE));
	return elektra_settings_write_string (backend, key, g_strconcat (G_ELEKTRA_SETTINGS_USER, G_ELEKTRA_SETTINGS_SW, key, NULL), value,
				       origin_tag);
}

static gint elektra_settings_keyset_from_tree (gpointer key, gpointer value, gpointer data)
{
	gchar * fullpathname = g_strconcat (G_ELEKTRA_SETTINGS_SW, (gchar *)(key), NULL);
	gchar * string_value = (value != NULL ? g_variant_print ((GVariant *)value, FALSE) : NULL);
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s: %s", "Append to keyset ", fullpathname, string_value);
	GElektraKeySet * gks = (GElektraKeySet *) data;
	GElektraKey * gkey = gelektra_keyset_lookup_byname(gks, fullpathname ,GELEKTRA_KDB_O_NONE);
	if (gkey == NULL)
	{
		gkey = gelektra_key_new (fullpathname, KEY_VALUE, string_value, KEY_END);
		if (gkey == NULL || gelektra_keyset_append (gks, gkey) == -1) return FALSE;
	}
	else
	{
		// Should we check if correct value is set?
		gelektra_key_setstring (gkey, string_value);
	}
	return FALSE;
}

/* elektra_settings_backend_write_tree implements g_settings_backend_write_tree:
 * @backend: a #GSettingsBackend implementation
 * @tree: a #GTree containing key-value pairs to write
 * @origin_tag: the origin tag
 *
 * Writes one or more keys.  This call will never block.
 *
 * The key of each item in the tree is the key name to write to and the
 * value is a #GVariant to write.  The proper type of #GTree for this
 * call can be created with g_settings_backend_create_tree().  This call
 * might take a reference to the tree; you must not modified the #GTree
 * after passing it to this call.
 *
 * This call does not fail.  During this call a #GSettingsBackend::changed
 * signal will be emitted if any keys have been changed.  The new values of
 * all updated keys will be visible to any signal callbacks.
 *
 * One possible method that an implementation might deal with failures is
 * to emit a second "changed" signal (either during this call, or later)
 * to indicate that the affected keys have suddenly "changed back" to their
 * old values.
 */
static gboolean elektra_settings_backend_write_tree (GSettingsBackend * backend, GTree * tree, gpointer origin_tag)
{
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s", "function write tree. ", "We have to transform GTree to a Keyset.");
	ElektraSettingsBackend * esb = (ElektraSettingsBackend *)backend;
  	g_tree_foreach (tree, elektra_settings_keyset_from_tree, esb->gks);
	g_settings_backend_changed_tree(backend, tree, origin_tag);
	return TRUE;
}

/* elektra_settings_backend_reset implements g_settings_backend_reset:
 * @backend: a #GSettingsBackend implementation
 * @key: the name of a key
 * @origin_tag: the origin tag
 *
 * "Resets" the named key to its "default" value (ie: after system-wide
 * defaults, mandatory keys, etc. have been taken into account) or possibly
 * unsets it.
 */
static void elektra_settings_backend_reset (GSettingsBackend * backend, const gchar * key, gpointer origin_tag)
{
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s", "Reset key:", key);
	ElektraSettingsBackend * esb = (ElektraSettingsBackend *)backend;
	gchar * keypathname = g_strconcat (G_ELEKTRA_SETTINGS_USER, G_ELEKTRA_SETTINGS_SW, key, NULL);
	GElektraKey * gkey = gelektra_keyset_lookup_byname (esb->gks, keypathname, GELEKTRA_KDB_O_NONE);
	g_free (keypathname);
	if (gkey != NULL)
	{ // TODO check on errors
		gelektra_keyset_lookup(esb->gks, gkey, KDB_O_POP);
	}
}

/* elektra_settings_backend_get_writable implements g_settings_backend_get_writable:
 * @backend: a #GSettingsBackend implementation
 * @key: the name of a key
 *
 * Finds out if a key is available for writing to.  This is the
 * interface through which 'lockdown' is implemented.  Locked down
 * keys will have %FALSE returned by this call.
 *
 * You should not write to locked-down keys, but if you do, the
 * implementation will deal with it.
 *
 * Returns: %TRUE if the key is writable
 */
static gboolean elektra_settings_backend_get_writable (GSettingsBackend * backend, const gchar * name)
{
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s %s", "Is key:", name, "writable?");

	ElektraSettingsBackend * esb = (ElektraSettingsBackend *)backend;

	gchar * pathToWrite = g_strconcat (G_ELEKTRA_SETTINGS_USER, G_ELEKTRA_SETTINGS_SW, name, NULL);
	GElektraKey * gkey = gelektra_keyset_lookup_byname (esb->gks, pathToWrite, GELEKTRA_KDB_O_NONE);
	if (gkey == NULL)
	{
		gkey = gelektra_key_new (pathToWrite, KEY_VALUE, G_ELEKTRA_TEST_STRING, KEY_END);
		g_free (pathToWrite);
		if (gkey == NULL) return FALSE;
	}
	return TRUE;
}

/* elektra_settings_backend_subscribe implements g_settings_backend_subscribe:
 * @backend: a #GSettingsBackend
 * @name: a key or path to subscribe to
 *
 * Requests that change signals be emitted for events on @name.
 */
static void elektra_settings_backend_subscribe (GSettingsBackend * backend, const gchar * name)
{
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s", "Subscribe to:", name);
}

/* elektra_settings_backend_unsubscribe implements g_settings_backend_unsubscribe:
 * @backend: a #GSettingsBackend
 * @name: a key or path to subscribe to
 *
 * Reverses the effect of a previous call to
 * g_settings_backend_subscribe().
 */
static void elektra_settings_backend_unsubscribe (GSettingsBackend * backend, const gchar * name)
{
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s", "Unsubscribe:", name);
}

static void elektra_settings_backend_sync (GSettingsBackend * backend)
{
	ElektraSettingsBackend * esb = (ElektraSettingsBackend *)backend;
	if (gelektra_kdb_set (esb->gkdb, esb->gks, esb->gkey) == -1 || gelektra_kdb_get (esb->gkdb, esb->gks, esb->gkey) == -1)
	{
		g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s\n", "Error on sync!");
		return;
	}
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s\n", "Sync state");
}

static void elektra_settings_backend_free_weak_ref (gpointer data)
{
}

/*
 * Open elektra on empty level, as we asume that any application using GSettings is
 * an application and GSettings itself does not offer any such distinction. In addition
 * we dont know the application path in advance.
 */
static void elektra_settings_backend_init (ElektraSettingsBackend * esb)
{
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", "Init new ElektraSettingsBackend");
	esb->gkey = gelektra_key_new ("", KEY_CASCADING_NAME, KEY_END);
	esb->gkdb = gelektra_kdb_open (esb->gkey);
	esb->gks = gelektra_keyset_new (0, GELEKTRA_KEYSET_END);
	gelektra_kdb_get (esb->gkdb, esb->gks, esb->gkey);
}

/*
 * Cleanup
 */
static void elektra_settings_backend_finalize (GObject * object)
{	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s", "Finalize ElektraSettingsBackend");
	ElektraSettingsBackend * esb = (ElektraSettingsBackend *)object;
	GElektraKey * errorkey;
	gelektra_kdb_close (esb->gkdb, errorkey);
	G_OBJECT_CLASS (elektra_settings_backend_parent_class)->finalize (object);
}

static void elektra_settings_backend_class_init (GSettingsBackendClass * class)
{
	GObjectClass * object_class = G_OBJECT_CLASS (class);

	object_class->finalize = elektra_settings_backend_finalize;

	class->read = elektra_settings_backend_read;
	class->read_user_value = elektra_settings_backend_read_user_value;
	class->write = elektra_settings_backend_write;
	class->write_tree = elektra_settings_backend_write_tree;
	class->reset = elektra_settings_backend_reset;
	class->get_writable = elektra_settings_backend_get_writable;
	class->subscribe = elektra_settings_backend_subscribe;
	class->unsubscribe = elektra_settings_backend_unsubscribe;
	class->sync = elektra_settings_backend_sync;
}

void g_io_module_load (GIOModule * module)
{
	g_type_module_use (G_TYPE_MODULE (module));
	g_io_extension_point_implement (G_SETTINGS_BACKEND_EXTENSION_POINT_NAME, elektra_settings_backend_get_type (), "elektra", 200);
}

void g_io_module_unload (GIOModule * module)
{
	g_assert_not_reached ();
}

gchar ** g_io_module_query (void)
{
	return g_strsplit (G_SETTINGS_BACKEND_EXTENSION_POINT_NAME, "!", 0);
}

#undef G_LOG_DOMAIN

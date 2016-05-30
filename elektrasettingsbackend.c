#define G_SETTINGS_ENABLE_BACKEND
#include <gio/gio.h>
#include <gio/gsettingsbackend.h>
#include <glib.h>

#include <elektra/gelektra-kdb.h>
#include <elektra/gelektra-key.h>
#include <elektra/gelektra-keyset.h>

#include <string.h>

#undef G_LOG_DOMAIN
#define G_LOG_DOMAIN "ElektraSettings"

typedef GSettingsBackendClass ElektraSettingsBackendClass;

typedef struct
{
	GSettingsBackend backend;
} ElektraSettingsBackend;

/**
 * SECTION:gsettingsbackend
 * @title: GSettingsBackend
 * @short_description: Interface for settings backend implementations
 * @include: gio/gsettingsbackend.h
 * @see_also: #GSettings, #GIOExtensionPoint
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

/*< private >
 * g_settings_backend_read:
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
  gchar * typestring = g_variant_type_dup_string(expected_type);
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s\n%s %s\n%s %s%s\n", "function read key:", key, "expected_type is:",
	       typestring, "and we", (default_value ? "" : "do not "), "want the default_value");
  gfree(typestring);
}

/*< private >
 * g_settings_backend_read_user_value:
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
	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s\n%s %s\n", "function read (user) key:", key, "expected_type is:",
	       g_variant_get_type_string (expected_type));
}

/*< private >
 * g_settings_backend_write:
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
  	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s\n%s %s\n", "function write key: ", key, "value is:", g_variant_print(value, TRUE));
}

/*< private >
 * g_settings_backend_write_tree:
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
  	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s\n", "function write tree. ", "We have to transform GTree to a Keyset.");
}

/*< private >
 * g_settings_backend_reset:
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
  	g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s\n", "Reset key:", key);
}

/*< private >
 * g_settings_backend_get_writable:
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
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s %s\n", "Is key:", name, "writable?");
}

/*< private >
 * g_settings_backend_subscribe:
 * @backend: a #GSettingsBackend
 * @name: a key or path to subscribe to
 *
 * Requests that change signals be emitted for events on @name.
 */
static void elektra_settings_backend_subscribe (GSettingsBackend * backend, const gchar * name)
{
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s\n", "Subscribe to:", name);
}

/*< private >
 * g_settings_backend_unsubscribe:
 * @backend: a #GSettingsBackend
 * @name: a key or path to subscribe to
 *
 * Reverses the effect of a previous call to
 * g_settings_backend_subscribe().
 */
static void elektra_settings_backend_unsubscribe (GSettingsBackend * backend, const gchar * name)
{
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s %s\n", "Unsubscribe:", name);
}

static void elektra_settings_backend_sync (GSettingsBackend * backend)
{
  g_log (G_LOG_DOMAIN, G_LOG_LEVEL_DEBUG, "%s\n", "Sync state");
}

static void elektra_settings_backend_free_weak_ref (gpointer data)
{
}

static void elektra_settings_backend_init (ElektraSettingsBackend * dcsb)
{
}

static void elektra_settings_backend_finalize (GObject * object)
{
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
	g_io_extension_point_implement (G_SETTINGS_BACKEND_EXTENSION_POINT_NAME, elektra_settings_backend_get_type (), "elektra", 100);
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

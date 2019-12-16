#ifndef ELEKTRA_SETTINGS_H
#define ELEKTRA_SETTINGS_H

#include <gio/gsettingsbackend.h>
#include <glib.h>

/*< private >
 * elektra_settings_read_string:
 * @backend: the #GSettingsBackend implementation ElektraSettingsBackend
 * @keypathname: the full keypath to read from Elektra
 * @expected_type: the expected #GVariantType to read as value from Elektra
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
 * Returns: the value that was parsed as GVariant, or %NULL
 */
static GVariant * elektra_settings_read_string (GSettingsBackend * backend, gchar * keypathname, const GVariantType * expected_type);

/* < private >
 * elektra_settings_write_string:
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
static gboolean elektra_settings_write_string (GSettingsBackend * backend, const gchar * key, gchar * keypathname, GVariant * value,
					       gpointer origin_tag);
void elektra_settings_check_bus_connection (GSettingsBackend * backend);
void (*GAsyncReadyCallback) elektra_settings_bus_connected (GObject * source_object, GAsyncResult * res, gpointer user_data);
void (*GDBusSignalCallback)
	elektra_settings_key_changed (GDBusConnection * connection, const gchar * sender_name, const gchar * object_path,
				      const gchar * interface_name, const gchar * signal_name, GVariant * parameters, gpointer user_data);
#endif ELEKTRA_SETTINGS_H

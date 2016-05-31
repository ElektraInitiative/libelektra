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

/*< private >
 * elektra_settings_write_string:
 * @backend: the #GSettingsBackend implementation ElektraSettingsBackend
 * @key: the key to write (used for notification)
 * @keypathname: the full keypath to write to Elektra
 * @value: the value to write as #GVariantType
 * @origin_tag: the origin tag (used for notification)
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
static gboolean elektra_settings_write_string (GSettingsBackend * backend, const gchar * key, gchar * keypathname, GVariant * value, gpointer origin_tag);
#endif ELEKTRA_SETTINGS_H

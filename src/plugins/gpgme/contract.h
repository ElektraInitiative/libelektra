/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME, KEY_VALUE, "gpgme plugin waits for your orders", KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open", KEY_FUNC, elektraGpgmeOpen, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close", KEY_FUNC, elektraGpgmeClose, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get", KEY_FUNC, elektraGpgmeGet, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set", KEY_FUNC, elektraGpgmeSet, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkconf", KEY_FUNC, elektraGpgmeCheckconf, KEY_END),
#include ELEKTRA_README
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version", KEY_VALUE, PLUGINVERSION, KEY_END),

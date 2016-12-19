/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

// clang-format off

ksNew (50,
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "",
		KEY_VALUE, "" ELEKTRA_PLUGIN_NAME " plugin waits for your orders", KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants", KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_SYSTEM",
		KEY_VALUE, KDB_DB_SYSTEM, KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_HOME",
		KEY_VALUE, KDB_DB_HOME, KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_USER",
		KEY_VALUE, KDB_DB_USER, KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_SPEC",
		KEY_VALUE, KDB_DB_SPEC, KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_DIR",
		KEY_VALUE, KDB_DB_DIR, KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/ELEKTRA_VARIANT_SYSTEM",
		KEY_VALUE, ELEKTRA_VARIANT_SYSTEM, KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/ELEKTRA_VARIANT_USER",
		KEY_VALUE, ELEKTRA_VARIANT_USER, KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/ELEKTRA_VARIANT_BASE",
		KEY_VALUE, ELEKTRA_VARIANT_BASE, KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, open),
		KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, close),
		KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, get),
		KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, set),
		KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/error",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, error),
		KEY_END),
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkfile",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, checkFile),
		KEY_END),
#include ELEKTRA_README(resolver)
	keyNew ("system/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version",
		KEY_VALUE, PLUGINVERSION, KEY_END),
	KS_END);

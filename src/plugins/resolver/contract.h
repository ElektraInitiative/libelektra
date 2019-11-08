/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

ksNew (50,
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "",
		KEY_VALUE, "" ELEKTRA_PLUGIN_NAME " plugin waits for your orders", KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants", KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_SYSTEM",
		KEY_VALUE, KDB_DB_SYSTEM, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_HOME",
		KEY_VALUE, KDB_DB_HOME, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_USER",
		KEY_VALUE, KDB_DB_USER, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_SPEC",
		KEY_VALUE, KDB_DB_SPEC, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_DIR",
		KEY_VALUE, KDB_DB_DIR, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/ELEKTRA_VARIANT_SYSTEM",
		KEY_VALUE, ELEKTRA_VARIANT_SYSTEM, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/ELEKTRA_VARIANT_USER",
		KEY_VALUE, ELEKTRA_VARIANT_USER, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/ELEKTRA_VARIANT_BASE",
		KEY_VALUE, ELEKTRA_VARIANT_BASE, KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(open),
		KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(close),
		KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(get),
		KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(set),
		KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/commit",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(commit),
		KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/error",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(error),
		KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkfile",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(checkFile),
		KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/filename",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(filename),
		KEY_END),
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/freeHandle",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(freeHandle),
		KEY_END),
#include ELEKTRA_README
	keyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version",
		KEY_VALUE, PLUGINVERSION, KEY_END),
	KS_END);

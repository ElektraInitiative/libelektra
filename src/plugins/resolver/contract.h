/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

elektraKeysetNew (50,
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "",
		ELEKTRA_KEY_VALUE, "" ELEKTRA_PLUGIN_NAME " plugin waits for your orders", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_SYSTEM",
		ELEKTRA_KEY_VALUE, KDB_DB_SYSTEM, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_HOME",
		ELEKTRA_KEY_VALUE, KDB_DB_HOME, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_USER",
		ELEKTRA_KEY_VALUE, KDB_DB_USER, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_SPEC",
		ELEKTRA_KEY_VALUE, KDB_DB_SPEC, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/KDB_DB_DIR",
		ELEKTRA_KEY_VALUE, KDB_DB_DIR, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/ELEKTRA_VARIANT_SYSTEM",
		ELEKTRA_KEY_VALUE, ELEKTRA_VARIANT_SYSTEM, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/ELEKTRA_VARIANT_USER",
		ELEKTRA_KEY_VALUE, ELEKTRA_VARIANT_USER, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/constants/ELEKTRA_VARIANT_BASE",
		ELEKTRA_KEY_VALUE, ELEKTRA_VARIANT_BASE, ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports", ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/open",
		ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(open),
		ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/close",
		ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(close),
		ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/get",
		ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(get),
		ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/set",
		ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(set),
		ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/commit",
		ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(commit),
		ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/error",
		ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(error),
		ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/checkfile",
		ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(checkFile),
		ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/filename",
		ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(filename),
		ELEKTRA_KEY_END),
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/exports/freeHandle",
		ELEKTRA_KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(freeHandle),
		ELEKTRA_KEY_END),
#include ELEKTRA_README
	elektraKeyNew ("system:/elektra/modules/" ELEKTRA_PLUGIN_NAME "/infos/version",
		ELEKTRA_KEY_VALUE, PLUGINVERSION, ELEKTRA_KEY_END),
	ELEKTRA_KS_END);

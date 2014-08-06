ksNew (50,
	keyNew ("system/elektra/modules/resolver",
		KEY_VALUE, "resolver plugin waits for your orders", KEY_END),
	keyNew ("system/elektra/modules/resolver/constants", KEY_END),
	keyNew ("system/elektra/modules/resolver/constants/KDB_DB_SYSTEM",
		KEY_VALUE, KDB_DB_SYSTEM, KEY_END),
	keyNew ("system/elektra/modules/resolver/constants/KDB_DB_HOME",
		KEY_VALUE, KDB_DB_HOME, KEY_END),
	keyNew ("system/elektra/modules/resolver/constants/KDB_DB_USER",
		KEY_VALUE, KDB_DB_USER, KEY_END),
	keyNew ("system/elektra/modules/resolver/exports", KEY_END),
	keyNew ("system/elektra/modules/resolver/exports/open",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, open),
		KEY_END),
	keyNew ("system/elektra/modules/resolver/exports/close",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, close),
		KEY_END),
	keyNew ("system/elektra/modules/resolver/exports/get",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, get),
		KEY_END),
	keyNew ("system/elektra/modules/resolver/exports/set",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, set),
		KEY_END),
	keyNew ("system/elektra/modules/resolver/exports/error",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, error),
		KEY_END),
	keyNew ("system/elektra/modules/resolver/exports/checkfile",
		KEY_FUNC, ELEKTRA_PLUGIN_FUNCTION(resolver, checkFile),
		KEY_END),
	keyNew ("system/elektra/modules/resolver/infos",
		KEY_VALUE, "All information you want to know are in keys below", KEY_END),
	keyNew ("system/elektra/modules/resolver/infos/author",
		KEY_VALUE, "Markus Raab <elektra@markus-raab.org>", KEY_END),
	keyNew ("system/elektra/modules/resolver/infos/licence",
		KEY_VALUE, "BSD", KEY_END),
	keyNew ("system/elektra/modules/resolver/infos/description",
		KEY_VALUE,
"== Scope ==\n"
"\n"
"The resolver handles operating system dependent tasks.\n"
"One task is the resolving of the filenames for user and system (hence its name)\n"
"\n"
"\n"
"We have an optimistic approach. Locking is only used to detect\n"
"concurrent cooperative processes in the short moment between prepare and commit.\n"
"A conflict will be raised in that situation.\n"
"When processes do not lock the file it might be overwritten.\n"
"This is unavoidable because\n"
"such problems can only be detected in the commit phase when it is too late for\n"
"rollbacks.\n"
"\n"
"== Reading Configuration ==\n"
"\n"
" 1.) stat the file\n"
" 2.) check if the file stat has changed\n"
" 3.) remember the time (last update)\n"
"\n"
"\n"
"== Writing Configuration ==\n"
"\n"
"\n"
" 1.) Open the configuration file\n"
"     If not available recursively create directories and retry.\n"
" 1.) Try to lock the configuration file, if not possible -> conflict\n"
" 2.) Check the update time -> conflict\n"
" 3.) update the update time\n"
"\n"
"\n"
"\n"
"\n"
"\n"
"\n"
"\n"
		,KEY_END),
	keyNew ("system/elektra/modules/resolver/infos/provides",
		KEY_VALUE, "resolver", KEY_END),
	keyNew ("system/elektra/modules/resolver/infos/placements",
		KEY_VALUE, "rollback getresolver setresolver commit", KEY_END),
	keyNew ("system/elektra/modules/resolver/infos/needs",
		KEY_VALUE, "", KEY_END),
	keyNew ("system/elektra/modules/resolver/infos/version",
		KEY_VALUE, PLUGINVERSION, KEY_END),
	KS_END);

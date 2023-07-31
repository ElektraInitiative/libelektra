/**
 * @file
 *
 * @brief Implementation of kdb mount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <mountOdbc.h>

#include <kdb.h>

#include <cpp-main.h>

#define COMMAND_NAME "mountOdbc"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

void addMountOdbcSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Mount a new backend.", KEY_META, "command",
				   COMMAND_NAME, KEY_END));

	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/data-source-name", KEY_META, "description",
				   "The name of the ODBC data source as defined in odbc.ini", KEY_META, "args", "indexed", KEY_META,
				   "args/index", "0", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/user-name", KEY_META, "description",
				   "The username that should be used for connecting to the ODBC data source. If no username is needed, or "
				   "it is already specified in the ini-file for the configuration of the datasource, \"\" can be provided "
				   "for this argument.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/password", KEY_META, "description",
				   "The same rules as for user name also apply to this argument.", KEY_META, "args", "indexed", KEY_META,
				   "args/index", "2", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/table-name", KEY_META, "description",
				   "The name of the table in the data source where the keys and their values should be stored.", KEY_META,
				   "args", "indexed", KEY_META, "args/index", "3", KEY_END));
	ksAppendKey (spec,
		     keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/key-col-name", KEY_META, "description",
			     "The name of the column where the key-names should be stored. This should be the primary key of the table.",
			     KEY_META, "args", "indexed", KEY_META, "args/index", "4", KEY_END));
	ksAppendKey (
		spec,
		keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/value-col-name", KEY_META, "description",
			"The name of the column where the key-values (strings) should be stored. This column should support NULL-values.",
			KEY_META, "args", "indexed", KEY_META, "args/index", "5", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/meta-table-name", KEY_META, "description",
				   "The name of the table where the metadata for the keys should be stored.", KEY_META, "args", "indexed",
				   KEY_META, "args/index", "6", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mt-key-col-name", KEY_META, "description",
				   "The name of the column in the meta table where the key-name should be stored. This should be a foreign "
				   "key that refers to the column with the key-name of the other table.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "7", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mt-metakey-col-name", KEY_META, "description",
				   "The name of the column in the meta table where the name of the metakey should be stored. This column, "
				   "together with the column for the key-name, should define the primary key of the meta table.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "8", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mt-metavalue-col-name", KEY_META, "description",
				   "The name of the column in the meta table where the value (string) of the metakey should be stored. "
				   "This column should support NULL-values.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "9", KEY_END));
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/timeout", KEY_META, "description",
				   "The timeout (in seconds) that should be used when connecting to the data source. When passing '0', the "
				   "timeout gets disabled and the application could potentially wait forever. So use this option with "
				   "care! If you want to use a default timeout, you can just pass \"\" for this argument.",
				   KEY_META, "args", "indexed", KEY_META, "args/index", "10", KEY_END));
	ksAppendKey (
		spec,
		keyNew (COMMAND_SPEC_KEY (COMMAND_NAME) "/mountpoint", KEY_META, "description",
			"The place in the KDB where the ODBC data source should be mounted. The syntax is the same as with the file-based "
			"backend, but you can only use user:/ and system:/ namespaces as mountpoints for ODBC data sources.",
			KEY_META, "args", "indexed", KEY_META, "args/index", "11", KEY_END));

	ADD_BASIC_OPTIONS (spec, COMMAND_SPEC_KEY (COMMAND_NAME))
}

int execCppMountOdbc (int argc, char ** argv)
{
	return cpp_main (argc, argv);
}

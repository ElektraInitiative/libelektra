/**
 * @file
 *
 * @brief Implementation of kdb meta command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <meta.h>
#include <meta-get.h>
#include <meta-ls.h>
#include <meta-set.h>

#include <command.h>
#include <kdb.h>
#include <kdberrors.h>

#define COMMAND_NAME "meta"

#define GET_OPTION_KEY(options, name) GET_OPT_KEY (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)
#define GET_OPTION(options, name) GET_OPT (options, COMMAND_BASE_KEY (COMMAND_NAME) "/" name)

command metaSubcommands[] = {
	{ "get", addMetaGetSpec, execMetaGet },
	{ "ls", addMetaLsSpec, execMetaLs },
	{ "set", addMetaSetSpec, execMetaSet },
};

void addMetaSpec (KeySet * spec)
{
	ksAppendKey (spec, keyNew (COMMAND_SPEC_KEY (COMMAND_NAME), KEY_META, "description", "Manage meta keys.", KEY_META, "command",
				   COMMAND_NAME, KEY_END));

	for (unsigned long i = 0; i < sizeof (metaSubcommands) / sizeof (metaSubcommands[0]); ++i)
	{
		metaSubcommands[i].addSpec (spec);
	}
}

int execMeta (KeySet * options, Key * errorKey)
{
	const char * subcommand = keyString (ksLookupByName (options, CLI_BASE_KEY "/" COMMAND_NAME, 0));

	for (unsigned long i = 0; i < sizeof (metaSubcommands) / sizeof (metaSubcommands[0]); ++i)
	{
		if (elektraStrCmp (subcommand, metaSubcommands[i].name) == 0)
		{
			return metaSubcommands[i].exec (options, errorKey);
		}
	}
	// this cannot happen, since not valid sub-commands are already detected when parsing
	return 1;

}

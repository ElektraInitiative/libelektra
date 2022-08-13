/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <command.h>
#include <kdb.h>
#include <kdbhelper.h>
#include <kdbopts.h>
#include <stdio.h>
#include <stdlib.h>

#define CLI_SPEC_KEY "spec:" CLI_BASE_KEY

extern char ** environ;

command subcommands[] = {};

void printWarnings (Key * errorKey)
{
	const char * warnings = keyString (keyGetMeta (errorKey, "warnings"));
	warnings = warnings[1] == '_' ? warnings + 2 : warnings + 1;

	int warningsCount = atoi (warnings);
	char buff[8 + 1 + 1 + 11 + 1 + 6 + 1];
	for (int i = 0; i <= warningsCount; ++i)
	{
		snprintf (buff, sizeof buff, "warnings/#%d/reason", i);
		const char * warning = keyString (keyGetMeta (errorKey, buff));
		printf ("WARNING: %s\n", warning);
	}
}

int main (int argc, char ** argv)
{
	KeySet * options = ksNew (1,
				  keyNew (CLI_SPEC_KEY, KEY_META, "command", "", KEY_META, "description",
					  "kdb is a program to manage Elektra's key database.", KEY_END),
				  KS_END);

	for (unsigned long i = 0; i < sizeof (subcommands) / sizeof (subcommands[0]); ++i)
	{
		subcommands[i].addSpec (options);
	}

	Key * errorKey = keyNew (CLI_SPEC_KEY, KEY_END);

	int result = elektraGetOpts (options, argc, (const char **) argv, (const char **) environ, errorKey);
	if (result == -1)
	{
		// there was an error
		fprintf (stderr, "ERROR: %s\n", keyString (keyGetMeta (errorKey, "error/reason")));
		keyDel (errorKey);
		ksDel (options);
		return 1;
	}

	const char * subcommand = keyString (ksLookupByName (options, CLI_BASE_KEY, 0));
	if (elektraStrCmp (subcommand, "") == 0)
	{
		fprintf (stderr, "kdb is a program to manage Elektra's key database. For more information use --help.\n\n");
	}

	if (result == 1)
	{
		// '--help' option was used
		char * help = elektraGetOptsHelpMessage (errorKey, NULL, NULL);
		fprintf (stderr, "%s\n", help);
		elektraFree (help);
		keyDel (errorKey);
		ksDel (options);
		return 0;
	}
	keyDel (errorKey);
	for (unsigned long i = 0; i < sizeof (subcommands) / sizeof (subcommands[0]); ++i)
	{
		if (elektraStrCmp (subcommand, subcommands[i].name) == 0)
		{
			errorKey = keyNew (CLI_SPEC_KEY, KEY_END);
			result = subcommands[i].checkArgs (options, errorKey);
			if (result != 0)
			{
				fprintf (stderr, "ARG ERROR: %s\n", GET_ERR (errorKey));
				keyDel (errorKey);
				ksDel (options);
				return 1;
			}
			result = subcommands[i].exec (options, errorKey);
			if (result != 0)
			{
				fprintf (stderr, "ERROR: %s\n", GET_ERR (errorKey));
			}
			printWarnings (errorKey);
			keyDel (errorKey);
			ksDel (options);
			return result;
		}
	}
	keyDel (errorKey);
	ksDel (options);
	return 0;
}

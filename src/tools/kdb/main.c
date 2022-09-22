/**
 * @file
 *
 * @brief The KDB cli tool
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <get.h>
#include <ls.h>

#include <command.h>
#include <kdb.h>
#include <kdbhelper.h>
#include <kdbopts.h>
#include <stdio.h>
#include <stdlib.h>

#include <cpp-main.h>

#define CLI_SPEC_KEY "spec:" CLI_BASE_KEY

extern char ** environ;

command subcommands[] = {
	{ "get", addGetSpec, execGet },
	{ "ls", addLsSpec, execLs },
};

void printError (Key * errorKey)
{
	if (HAS_ERR (errorKey) && HAS_ERR_CODE (errorKey))
	{
		fprintf (stderr, "ERROR [%s]: %s: %s\n", GET_ERR_CODE (errorKey), GET_ERR_DESC (errorKey), GET_ERR (errorKey));
	}
	else if (HAS_ERR (errorKey))
	{
		fprintf (stderr, "ERROR: %s: %s\n", GET_ERR_DESC (errorKey), GET_ERR (errorKey));
	}
}

void printWarnings (Key * errorKey)
{
	const Key * warningsKey = keyGetMeta (errorKey, "warnings");
	if (warningsKey == NULL)
	{
		return;
	}
	const char * warnings = keyString (warningsKey);
	warnings = warnings[1] == '_' ? warnings + 2 : warnings + 1;

	int warningsCount = atoi (warnings);
	char buff[8 + 1 + 1 + 11 + 1 + 6 + 1];
	for (int i = 0; i <= warningsCount; ++i)
	{
		snprintf (buff, sizeof buff, "warnings/#%d/reason", i);
		const char * reason = keyString (keyGetMeta (errorKey, buff));

		snprintf (buff, sizeof buff, "warnings/#%d/number", i);
		const Key * tmp = keyGetMeta (errorKey, buff);
		if (tmp != NULL)
		{
			const char * number = keyString (keyGetMeta (errorKey, buff));
			fprintf (stderr, "WARNING [%s]: %s\n", number, reason);
		}
		else
		{
			fprintf (stderr, "WARNING: %s\n", reason);
		}
	}
}

void printVersion (void)
{
	KeySet * versions = ksNew (0, KS_END);
	Key * parentKey = keyNew ("system:/elektra/version", KEY_END);
	KDB * kdb = kdbOpen (NULL, parentKey);
	kdbGet (kdb, versions, parentKey);

	Key * kdb_version = ksLookupByName (versions, "system:/elektra/version/constants/KDB_VERSION", 0);
	if (!kdb_version)
	{
		fprintf (stderr, "Could not lookup KDB_VERSION key\n");
	}
	else
	{
		printf ("KDB_VERSION: %s\n", keyString (kdb_version));
	}

	Key * so_version = ksLookupByName (versions, "system:/elektra/version/constants/SO_VERSION", 0);
	if (!so_version)
	{
		fprintf (stderr, "Could not lookup SO_VERSION key\n");
	}
	else
	{
		printf ("SO_VERSION: %s\n", keyString (so_version));
	}

	keyDel (parentKey);
	ksDel (versions);
	kdbClose (kdb, NULL);
}


int main (int argc, char ** argv)
{
	if (argc == 2 && (elektraStrCmp (argv[1], "--version") == 0 || elektraStrCmp (argv[1], "-V") == 0))
	{
		printVersion ();
		return 0;
	}

	Key * parentKey = keyNew (CLI_SPEC_KEY, KEY_END);
	KeySet * options = ksNew (1,
				  keyNew (CLI_SPEC_KEY, KEY_META, "command", "", KEY_META, "description",
					  "kdb is a program to manage Elektra's key database.", KEY_END),
				  KS_END);

	for (unsigned long i = 0; i < sizeof (subcommands) / sizeof (subcommands[0]); ++i)
	{
		subcommands[i].addSpec (options);
	}

	int result = elektraGetOpts (options, argc, (const char **) argv, (const char **) environ, parentKey);
	if (result == 1)
	{ // --help
		const char * helpMessage = elektraGetOptsHelpMessage (parentKey, NULL, NULL);
		fprintf (stderr, "%s\n", helpMessage);
		printWarnings (parentKey);
		keyDel (parentKey);
		ksDel (options);
		return 0;
	}
	if (result == -1)
	{ // error
		const char * errorMessage = GET_ERR (parentKey);
		if (elektraStrNCmp (errorMessage, "Unknown sub-command:", 20) == 0)
		{
			goto cpp;
		}
		if (elektraStrNCmp (errorMessage, "Unknown short option:", 21) == 0 ||
		    elektraStrNCmp (errorMessage, "Unknown long option:", 20) == 0)
		{
			result = 1;
		}
		else
		{
			result = 5;
		}
		fprintf (stderr, "ERROR: %s\n", errorMessage);

		goto cleanup;
	}

	const char * subcommand = keyString (ksLookupByName (options, CLI_BASE_KEY, 0));
	if (elektraStrCmp (subcommand, "") == 0)
	{
		printf ("kdb is a program to manage Elektra's key database. For more information use --help.\n\n");
		result = EXIT_SUCCESS;
		goto cleanup;
	}

	for (unsigned long i = 0; i < sizeof (subcommands) / sizeof (subcommands[0]); ++i)
	{
		if (elektraStrCmp (subcommand, subcommands[i].name) == 0)
		{
			keyDel (parentKey);
			parentKey = keyNew (CLI_BASE_KEY, KEY_END);
			result = subcommands[i].exec (options, parentKey);
			printError (parentKey);
			printWarnings (parentKey);
			goto cleanup;
		}
	}
cpp:
	result = cpp_main (argc, argv);

cleanup:
	keyDel (parentKey);
	ksDel (options);
	return result;
}

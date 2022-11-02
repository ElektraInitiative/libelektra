/**
 * @file
 *
 * @brief The KDB cli tool
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <basename.h>
#include <cmerge.h>
#include <dirname.h>
#include <get.h>
#include <ls.h>
#include <mountpoint.h>
#include <namespace.h>

#include <command.h>
#include <kdb.h>
#include <kdbgopts.h>
#include <kdbhelper.h>
#include <kdbinvoke.h>
#include <stdio.h>
#include <stdlib.h>

#define CLI_SPEC_KEY "spec:" CLI_BASE_KEY

extern char ** environ;

command subcommands[] = {
	{ "basename", addBasenameSpec, execBasename },
	{ "cmerge", addCmergeSpec, execCmerge },
	{ "dirname", addDirnameSpec, execDirname },
	{ "get", addGetSpec, execGet },
	{ "ls", addLsSpec, execLs },
	{ "mountpoint", addMountpointSpec, execMountpoint },
	{ "namespace", addNamespaceSpec, execNamespace },
};

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
		const char * warning = keyString (keyGetMeta (errorKey, buff));
		printf ("WARNING: %s\n", warning);
	}
}

int main (int argc, char ** argv)
{
	if (argc == 2 && elektraStrCmp (argv[1], "--elektra-spec") == 0)
	{
		Key * parentKey = keyNew (CLI_SPEC_KEY, KEY_END);
		KeySet * options = ksNew (1,
					  keyNew (CLI_SPEC_KEY, KEY_META, "command", "", KEY_META, "description",
						  "kdb is a program to manage Elektra's key database.", KEY_END),
					  KS_END);

		for (unsigned long i = 0; i < sizeof (subcommands) / sizeof (subcommands[0]); ++i)
		{
			subcommands[i].addSpec (options);
		}

		KeySet * specloadConf = ksNew (1, keyNew ("system:/sendspec", KEY_END), KS_END);
		ElektraInvokeHandle * specload = elektraInvokeOpen ("specload", specloadConf, parentKey);
		int result = elektraInvoke2Args (specload, "sendspec", options, parentKey);

		elektraInvokeClose (specload, parentKey);
		keyDel (parentKey);
		ksDel (specloadConf);
		return result;
	}

	Key * parentKey = keyNew (CLI_BASE_KEY, KEY_END);
	KeySet * options = ksNew (0, KS_END);

	KeySet * goptsConfig = ksNew (0, KS_END);
	KeySet * contract = ksNew (0, KS_END);

	elektraGOptsContract (contract, argc, (const char * const *) argv, (const char * const *) environ, parentKey, goptsConfig);

	KDB * kdb = kdbOpen (contract, parentKey);

	ksDel (goptsConfig);
	ksDel (contract);

	int result = kdbGet (kdb, options, parentKey);

	const char * subcommand = keyString (ksLookupByName (options, CLI_BASE_KEY, 0));
	if (elektraStrCmp (subcommand, "") == 0)
	{
		printf ("kdb is a program to manage Elektra's key database. For more information use --help.\n\n");
		result = EXIT_SUCCESS;
		goto cleanup;
	}

	Key * helpKey = ksLookupByName (options, "proc:/elektra/gopts/help", 0);
	if (helpKey != NULL && elektraStrCmp (keyString (helpKey), "1") == 0)
	{
		const char * help = keyString (ksLookupByName (options, "proc:/elektra/gopts/help/message", 0));
		printf ("%s\n", help);
		result = EXIT_SUCCESS;
		goto cleanup;
	}

	if (result == -1 || HAS_ERR (parentKey))
	{
		fprintf (stderr, "ERROR: %s\n", GET_ERR (parentKey));
		result = EXIT_FAILURE;
		goto cleanup;
	}

	for (unsigned long i = 0; i < sizeof (subcommands) / sizeof (subcommands[0]); ++i)
	{
		if (elektraStrCmp (subcommand, subcommands[i].name) == 0)
		{
			keyDel (parentKey);
			parentKey = keyNew (CLI_BASE_KEY, KEY_END);
			result = subcommands[i].exec (options, parentKey);
			if (HAS_ERR (parentKey))
			{
				fprintf (stderr, "ERROR: %s\n", GET_ERR (parentKey));
			}
			printWarnings (parentKey);
			goto cleanup;
		}
	}
	fprintf (stderr, "ERROR: \'help\' is not a valid subcommand, use --help for more information.\n");


cleanup:
	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	ksDel (options);
	return result;
}

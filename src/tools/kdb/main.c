/**
 * @file
 *
 * @brief The KDB cli tool
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <basename.h>
#include <cache.h>
#include <complete.h>
#include <convert.h>
#include <cp.h>
#include <dirname.h>
#include <editor.h>
#include <export.h>
#include <file.h>
#include <find.h>
#include <gen.h>
#include <get.h>
#include <import.h>
#include <list-tools.h>
#include <ls.h>
#include <merge.h>
#include <meta-get.h>
#include <meta-ls.h>
#include <meta-rm.h>
#include <meta-set.h>
#include <meta-show.h>
#include <mount.h>
#include <mv.h>
#include <namespace.h>
#include <plugin-check.h>
#include <plugin-info.h>
#include <plugin-list.h>
#include <record-export.h>
#include <record-reset.h>
#include <record-rm.h>
#include <record-start.h>
#include <record-state.h>
#include <record-stop.h>
#include <record-undo.h>
#include <remount.h>
#include <rm.h>
#include <set.h>
#include <sget.h>
#include <shell.h>
#include <spec-mount.h>
#include <test.h>
#include <umount.h>
#include <validate.h>

#include <command.h>
#include <external.h>
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
	{ "record-start", addRecordStartSpec, execRecordStart },
	{ "record-undo", addRecordUndoSpec, execRecordUndo },
	{ "set", addSetSpec, execSet },
};

cppCommand cppSubcommands[] = {
	{ "basename", addBasenameSpec, execCppBasename },
	{ "cp", addCpSpec, execCppCp },
	{ "dirname", addDirnameSpec, execCppDirname },
	{ "find", addFindSpec, execCppFind },
	{ "ls", addLsSpec, execCppLs },
	{ "merge", addMergeSpec, execCppMerge },
	{ "meta-get", addMetaGetSpec, execCppMetaGet },
	{ "meta-ls", addMetaLsSpec, execCppMetaLs },
	{ "meta-rm", addMetaRmSpec, execCppMetaRm },
	{ "meta-set", addMetaSetSpec, execCppMetaSet },
	{ "meta-show", addMetaShowSpec, execCppMetaShow },
	{ "mv", addMvSpec, execCppMv },
	{ "namespace", addNamespaceSpec, execCppNamespace },
	{ "rm", addRmSpec, execCppRm },
	{ "sget", addSgetSpec, execCppSget },
	{ "cache", addCacheSpec, execCppCache },
	{ "complete", addCompleteSpec, execCppComplete },
	{ "convert", addConvertSpec, execCppConvert },
	{ "editor", addEditorSpec, execCppEditor },
	{ "file", addFileSpec, execCppFile },
	{ "gen", addGenSpec, execCppGen },
	{ "list-tools", addListToolsSpec, execCppListTools },
	{ "plugin-check", addPluginCheckSpec, execCppPluginCheck },
	{ "plugin-info", addPluginInfoSpec, execCppPluginInfo },
	{ "plugin-list", addPluginListSpec, execCppPluginList },
	{ "remount", addRemountSpec, execCppRemount },
	{ "record-export", addRecordExportSpec, execCppRecordExport },
	{ "record-reset", addRecordResetSpec, execCppRecordReset },
	{ "record-rm", addRecordRmSpec, execCppRecordRm },
	{ "record-state", addRecordStateSpec, execCppRecordState },
	{ "record-stop", addRecordStopSpec, execCppRecordStop },
	{ "shell", addShellSpec, execCppShell },
	{ "spec-mount", addSpecMountSpec, execCppSpecMount },
	{ "test", addTestSpec, execCppTest },
	{ "umount", addUmountSpec, execCppUmount },
	{ "validate", addValidateSpec, execCppValidate },
};

cppCommand cppSubcommandsNoExec[] = {
	{ "export", addExportSpec, NULL },
	{ "import", addImportSpec, NULL },
	{ "mount", addMountSpec, NULL },
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
	if (argc == 1)
	{
		printf ("kdb is a program to manage Elektra's key database. For more information use --help.\n\n");
		return 0;
	}

	// commands where the cpp spec is not representable with libelektra opts, optional indexed args
	for (unsigned long i = 0; i < sizeof (cppSubcommandsNoExec) / sizeof (cppCommand); ++i)
	{
		if (cppSubcommandsNoExec[i].exec == NULL && elektraStrCmp (cppSubcommandsNoExec[i].name, argv[1]) == 0)
		{
			return cpp_main (argc, argv);
		}
	}


	Key * parentKey = keyNew (CLI_SPEC_KEY, KEY_END);
	KeySet * options = ksNew (1,
				  keyNew (CLI_SPEC_KEY, KEY_META, "command", "", KEY_META, "description",
					  "kdb is a program to manage Elektra's key database.", KEY_END),
				  KS_END);

	// external programs with spec in spec:/sw/elektra/kdb/#0/current/<cmd>
	KeySet * externalBinaries = ksNew (10, KS_END);
	loadExternalSpec (options, externalBinaries, parentKey);

	// C spec
	for (unsigned long i = 0; i < sizeof (subcommands) / sizeof (command); ++i)
	{
		subcommands[i].addSpec (options);
	}

	// C++ spec
	for (unsigned long i = 0; i < sizeof (cppSubcommands) / sizeof (cppCommand); ++i)
	{
		cppSubcommands[i].addSpec (options);
	}

	// no exec C++
	for (unsigned long i = 0; i < sizeof (cppSubcommandsNoExec) / sizeof (cppCommand); ++i)
	{
		cppSubcommandsNoExec[i].addSpec (options);
	}

	int result = elektraGetOpts (options, argc, (const char **) argv, (const char **) environ, parentKey);
	if (result == 1)
	{ // --help
		const char * helpMessage = elektraGetOptsHelpMessage (parentKey, NULL, NULL);
		fprintf (stderr, "%s\n", helpMessage);
		printWarnings (parentKey);
	        elektraFree ((void *) helpMessage);
		keyDel (parentKey);
		ksDel (options);
		ksDel (externalBinaries);
		return 0;
	}
	if (result == -1)
	{ // error
		const char * errorMessage = GET_ERR (parentKey);
		if (elektraStrNCmp (errorMessage, "Unknown sub-command:", 20) == 0)
		{
			if (tryLoadExternal (argv[1], externalBinaries) == 0)
			{
				result = 0;
			}
			else
			{
				result = 4;
			}
		}
		else if (elektraStrNCmp (errorMessage, "Unknown short option:", 21) == 0 ||
			 elektraStrNCmp (errorMessage, "Unknown long option:", 20) == 0)
		{
			result = 1;
		}
		else
		{
			result = 5;
		}
		if (result != 0 && (argc != 2 || elektraStrCmp (argv[1], "mount") != 0))
		{
			fprintf (stderr, "ERROR: %s\n", errorMessage);
			const char * helpMessage = elektraGetOptsHelpMessage (parentKey, NULL, NULL);
			fprintf (stderr, "\n%s\n", helpMessage);
			elektraFree ((void *) helpMessage);
			goto cleanup;
		}
	}

	const char * subcommand = keyString (ksLookupByName (options, CLI_BASE_KEY, 0));

	// external
	const char * externalBin = getExternalBin (externalBinaries, argv[1]);
	if (externalBin != NULL)
	{
		Key * errorKey = keyNew (CLI_SPEC_KEY, KEY_END);
		result = runExternal (externalBin, argv, errorKey);
		printError (errorKey);
		keyDel (errorKey);
		goto cleanup;
	}

	// C
	for (unsigned long i = 0; i < sizeof (subcommands) / sizeof (command); ++i)
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

	// C++
	for (unsigned long i = 0; i < sizeof (cppSubcommands) / sizeof (cppCommand); ++i)
	{
		if (elektraStrCmp (subcommand, cppSubcommands[i].name) == 0)
		{
			result = cppSubcommands[i].exec (argc, argv);
			goto cleanup;
		}
	}

cleanup:
	keyDel (parentKey);
	ksDel (options);
	ksDel (externalBinaries);
	return result;
}

/**
 * @file
 *
 * @brief Example for using command-line options with sub-commands
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <kdbease.h>
#include <kdbgopts.h>
#include <kdbhelper.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char ** environ;

#define BASE_KEY "/sw/org/kdbdummy/#0/current"
#define SPEC_BASE_KEY "spec" BASE_KEY

// -----------------
// Helper methods
// -----------------

/*
 * The methods below are only used, so that this example is self-contained.
 * If you actually develop an application, you may use the `specload` plugin,
 * but in any case the specification should be mounted into the KDB using `kdb mount`.
 *
 * DO NOT set/unset the specification inside of your application.
 */

static ElektraKeyset * createSpec (void)
{
	return elektraKeysetNew (10, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END),
		      elektraKeyNew (SPEC_BASE_KEY "/printversion", ELEKTRA_KEY_META, "description",
			      "print version information and exit (ignoring all other options/commands/parameters)", ELEKTRA_KEY_META, "opt", "v",
			      ELEKTRA_KEY_META, "opt/arg", "none", ELEKTRA_KEY_META, "opt/long", "version", ELEKTRA_KEY_END),
		      elektraKeyNew (SPEC_BASE_KEY "/getter", ELEKTRA_KEY_META, "description", "get a key's value", ELEKTRA_KEY_META, "command", "get", ELEKTRA_KEY_END),
		      elektraKeyNew (SPEC_BASE_KEY "/getter/verbose", ELEKTRA_KEY_META, "description",
			      "print additional information about where the value comes from", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META, "opt/long",
			      "verbose", ELEKTRA_KEY_META, "opt/arg", "none", ELEKTRA_KEY_END),
		      elektraKeyNew (SPEC_BASE_KEY "/getter/keyname", ELEKTRA_KEY_META, "description", "name of the key to read", ELEKTRA_KEY_META, "args",
			      "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END),
		      elektraKeyNew (SPEC_BASE_KEY "/setter", ELEKTRA_KEY_META, "description", "set a key's value", ELEKTRA_KEY_META, "command", "set", ELEKTRA_KEY_END),
		      elektraKeyNew (SPEC_BASE_KEY "/setter/verbose", ELEKTRA_KEY_META, "description",
			      "print additional information about where the value will be stored", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META,
			      "opt/long", "verbose", ELEKTRA_KEY_META, "opt/arg", "none", ELEKTRA_KEY_END),
		      elektraKeyNew (SPEC_BASE_KEY "/setter/keyname", ELEKTRA_KEY_META, "description", "name of the key to write", ELEKTRA_KEY_META, "args",
			      "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END),
		      elektraKeyNew (SPEC_BASE_KEY "/setter/value", ELEKTRA_KEY_META, "description", "value to be written", ELEKTRA_KEY_META, "args", "indexed",
			      ELEKTRA_KEY_META, "args/index", "1", ELEKTRA_KEY_END),
		      elektraKeyNew (SPEC_BASE_KEY "/dynamic/#", ELEKTRA_KEY_META, "description", "dynamically call a user-supplied command", ELEKTRA_KEY_META,
			      "args", "remaining", ELEKTRA_KEY_END),
		      ELEKTRA_KS_END);
}

static int setupSpec (void)
{
	ElektraKey * parentKey = elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END);
	ElektraKdb * kdb = elektraKdbOpen (NULL, parentKey);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKdbGet (kdb, ks, parentKey);

	ElektraKeyset * existing = elektraKeysetCut (ks, parentKey);
	if (elektraKeysetGetSize (existing) > 0)
	{
		elektraKdbClose (kdb, parentKey);
		elektraKeysetDel (ks);
		elektraKeysetDel (existing);
		return 0;
	}
	elektraKeysetDel (existing);

	ElektraKeyset * spec = createSpec ();
	elektraKeysetAppend (ks, spec);
	elektraKeysetDel (spec);
	elektraKdbSet (kdb, ks, parentKey);
	elektraKdbClose (kdb, parentKey);
	elektraKeysetDel (ks);

	return 1;
}


static void removeSpec (void)
{
	ElektraKey * parentKey = elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END);
	ElektraKdb * kdb = elektraKdbOpen (NULL, parentKey);
	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	elektraKdbGet (kdb, ks, parentKey);
	ElektraKeyset * spec = elektraKeysetCut (ks, parentKey);
	elektraKeysetDel (spec);
	elektraKdbSet (kdb, ks, parentKey);
	elektraKdbClose (kdb, parentKey);
	elektraKeysetDel (ks);
}

// -----------------
// Main example
// -----------------

int main (int argc, const char ** argv)
{
	// normally, you shouldn't mount the spec here
	// it should be mounted already
	// we do this just to keep the example self-contained
	if (!setupSpec ())
	{
		fprintf (stderr, "ERROR: Couldn't setup spec, keys exist!\n");
		return EXIT_FAILURE;
	}

	ElektraKey * parentKey = elektraKeyNew (BASE_KEY, ELEKTRA_KEY_END);
	ElektraKeyset * goptsConfig = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * contract = elektraKeysetNew (0, ELEKTRA_KS_END);

	elektraGOptsContract (contract, argc, argv, (const char * const *) environ, parentKey, goptsConfig);

	ElektraKdb * kdb = elektraKdbOpen (contract, parentKey);

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);
	int rc = elektraKdbGet (kdb, ks, parentKey);

	if (rc == -1)
	{
		fprintf (stderr, "ERROR: kdbGet failed! %s\n", elektraKeyString (elektraKeyGetMeta (parentKey, "error/reason")));
		elektraKdbClose (kdb, parentKey);
		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		removeSpec ();
		return EXIT_FAILURE;
	}

	ElektraKey * helpKey = elektraKeysetLookupByName (ks, "proc:/elektra/gopts/help", 0);
	if (helpKey != NULL && elektraStrCmp (elektraKeyString (helpKey), "1") == 0)
	{
		const char * help = elektraKeyString (elektraKeysetLookupByName (ks, "proc:/elektra/gopts/help/message", 0));
		printf ("%s\n", help);
		elektraKdbClose (kdb, parentKey);
		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		removeSpec ();
		return EXIT_SUCCESS;
	}

	printf ("A real implementation would now\n");

	ElektraKey * lookup = elektraKeysetLookupByName (ks, BASE_KEY "/printversion", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
	{
		printf ("print version information\n");

		elektraKdbClose (kdb, parentKey);
		elektraKeyDel (parentKey);
		elektraKeysetDel (ks);
		removeSpec ();
		return EXIT_SUCCESS;
	}

	lookup = elektraKeysetLookupByName (ks, BASE_KEY "", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "getter") == 0)
	{
		lookup = elektraKeysetLookupByName (ks, BASE_KEY "/getter/keyname", 0);
		if (lookup == NULL || strlen (elektraKeyString (lookup)) == 0)
		{
			printf ("report the error 'empty parameter: keyname'\n");
			elektraKdbClose (kdb, parentKey);
			elektraKeyDel (parentKey);
			elektraKeysetDel (ks);
			removeSpec ();
			return EXIT_SUCCESS;
		}
		printf ("get the key '%s'\n", elektraKeyString (lookup));

		lookup = elektraKeysetLookupByName (ks, BASE_KEY "/getter/verbose", 0);
		if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
		{
			printf ("print where the read key value comes from\n");
		}
	}
	else if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "setter") == 0)
	{
		const char * keyname;

		lookup = elektraKeysetLookupByName (ks, BASE_KEY "/setter/keyname", 0);
		if (lookup == NULL || strlen (keyname = elektraKeyString (lookup)) == 0)
		{
			printf ("report the error 'missing parameter: keyname'\n");
			elektraKdbClose (kdb, parentKey);
			elektraKeyDel (parentKey);
			elektraKeysetDel (ks);
			removeSpec ();
			return EXIT_SUCCESS;
		}
		lookup = elektraKeysetLookupByName (ks, BASE_KEY "/setter/value", 0);
		if (lookup == NULL)
		{
			printf ("report the error 'missing parameter: value'\n");
			elektraKdbClose (kdb, parentKey);
			elektraKeyDel (parentKey);
			elektraKeysetDel (ks);
			removeSpec ();
			return EXIT_SUCCESS;
		}
		printf ("set the key '%s' with the value '%s'\n", keyname, elektraKeyString (lookup));

		lookup = elektraKeysetLookupByName (ks, BASE_KEY "/setter/verbose", 0);
		if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
		{
			printf ("print where the key value is stored now\n");
		}
	}
	else
	{
		ElektraKey * arrayParent = elektraKeysetLookupByName (ks, BASE_KEY "/dynamic", 0);
		ElektraKeyset * dynamicCommand = elektraArrayGet (arrayParent, ks);

		if (elektraKeysetGetSize (dynamicCommand) > 0)
		{
			printf ("dynamically invoke the command '");
			elektraKeysetRewind (dynamicCommand);
			printf ("%s' with arguments:", elektraKeyString (elektraKeysetNext (dynamicCommand)));
			ElektraKey * cur = NULL;
			while ((cur = elektraKeysetNext (dynamicCommand)) != NULL)
			{
				printf (" %s", elektraKeyString (cur));
			}
			printf ("\n");
		}
		else
		{
			printf ("do nothing\n");
		}
	}

	elektraKdbClose (kdb, parentKey);
	elektraKeyDel (parentKey);
	elektraKeysetDel (ks);

	// normally, you shouldn't remove the spec,
	// because you shouldn't have mounted it inside the application,
	// we do this just to keep the example self-contained
	removeSpec ();

	return EXIT_SUCCESS;
}

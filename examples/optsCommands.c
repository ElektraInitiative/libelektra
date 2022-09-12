/**
 * @file
 *
 * @brief Advanced use example for elektraGetOpts (with sub-commands)
 *        You should prefer the example in goptsCommands.c over this one.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <kdbease.h>
#include <kdbhelper.h>
#include <kdbopts.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char ** environ;

#define BASE_KEY "/sw/org/kdbdummy/#0/current"
#define SPEC_BASE_KEY "spec" BASE_KEY

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

int main (int argc, const char ** argv)
{
	/**
	 * THIS IS AN EXAMPLE FOR AN ADVANCED USE CASE
	 *
	 * If you follow this example, please make sure
	 * you know what you are doing.
	 *
	 * Some of the functions used in this example,
	 * may not be part of the public API or may not
	 * be considered stable.
	 */


	ElektraKeyset * ks = createSpec ();
	ElektraKey * errorKey = elektraKeyNew (BASE_KEY, ELEKTRA_KEY_END);

	int result = elektraGetOpts (ks, argc, argv, (const char **) environ, errorKey);
	if (result == -1)
	{
		// there was an error
		fprintf (stderr, "ERROR: %s\n", elektraKeyString (elektraKeyGetMeta (errorKey, "error/reason")));
		elektraKeyDel (errorKey);
		elektraKeysetDel (ks);
		return EXIT_FAILURE;
	}

	if (result == 1)
	{
		// '--help' option was used
		char * help = elektraGetOptsHelpMessage (errorKey, NULL, NULL);
		fprintf (stderr, "%s\n", help);
		elektraFree (help);
		elektraKeyDel (errorKey);
		elektraKeysetDel (ks);
		return EXIT_SUCCESS;
	}

	printf ("A real implementation would now\n");

	ElektraKey * lookup = elektraKeysetLookupByName (ks, BASE_KEY "/printversion", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
	{
		printf ("print version information\n");

		elektraKeyDel (errorKey);
		elektraKeysetDel (ks);

		return EXIT_SUCCESS;
	}

	lookup = elektraKeysetLookupByName (ks, BASE_KEY "", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "getter") == 0)
	{
		lookup = elektraKeysetLookupByName (ks, BASE_KEY "/getter/keyname", 0);
		if (lookup == NULL || strlen (elektraKeyString (lookup)) == 0)
		{
			printf ("report the error 'empty parameter: keyname'\n");
			elektraKeyDel (errorKey);
			elektraKeysetDel (ks);

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
			elektraKeyDel (errorKey);
			elektraKeysetDel (ks);

			return EXIT_SUCCESS;
		}
		lookup = elektraKeysetLookupByName (ks, BASE_KEY "/setter/value", 0);
		if (lookup == NULL)
		{
			printf ("report the error 'missing parameter: value'\n");
			elektraKeyDel (errorKey);
			elektraKeysetDel (ks);

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

	elektraKeyDel (errorKey);
	elektraKeysetDel (ks);

	return EXIT_SUCCESS;
}

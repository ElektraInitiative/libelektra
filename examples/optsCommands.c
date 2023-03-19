/**
 * @file
 *
 * @brief Advanced use example for elektraGetOpts (with sub-commands)
 *        You should prefer the example in goptsCommands.c over this one.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/ease/old_ease.h>
#include <elektra/core/keyset.h>
#include <elektra/core/key.h>
#include <elektra/opts.h>
#include <internal/utility/old_helper.h>

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char ** environ;

#define BASE_KEY "/sw/org/kdbdummy/#0/current"
#define SPEC_BASE_KEY "spec" BASE_KEY

static KeySet * createSpec (void)
{
	return ksNew (10, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END),
		      keyNew (SPEC_BASE_KEY "/printversion", KEY_META, "description",
			      "print version information and exit (ignoring all other options/commands/parameters)", KEY_META, "opt", "v",
			      KEY_META, "opt/arg", "none", KEY_META, "opt/long", "version", KEY_END),
		      keyNew (SPEC_BASE_KEY "/getter", KEY_META, "description", "get a key's value", KEY_META, "command", "get", KEY_END),
		      keyNew (SPEC_BASE_KEY "/getter/verbose", KEY_META, "description",
			      "print additional information about where the value comes from", KEY_META, "opt", "v", KEY_META, "opt/long",
			      "verbose", KEY_META, "opt/arg", "none", KEY_END),
		      keyNew (SPEC_BASE_KEY "/getter/keyname", KEY_META, "description", "name of the key to read", KEY_META, "args",
			      "indexed", KEY_META, "args/index", "0", KEY_END),
		      keyNew (SPEC_BASE_KEY "/setter", KEY_META, "description", "set a key's value", KEY_META, "command", "set", KEY_END),
		      keyNew (SPEC_BASE_KEY "/setter/verbose", KEY_META, "description",
			      "print additional information about where the value will be stored", KEY_META, "opt", "v", KEY_META,
			      "opt/long", "verbose", KEY_META, "opt/arg", "none", KEY_END),
		      keyNew (SPEC_BASE_KEY "/setter/keyname", KEY_META, "description", "name of the key to write", KEY_META, "args",
			      "indexed", KEY_META, "args/index", "0", KEY_END),
		      keyNew (SPEC_BASE_KEY "/setter/value", KEY_META, "description", "value to be written", KEY_META, "args", "indexed",
			      KEY_META, "args/index", "1", KEY_END),
		      keyNew (SPEC_BASE_KEY "/dynamic/#", KEY_META, "description", "dynamically call a user-supplied command", KEY_META,
			      "args", "remaining", KEY_END),
		      KS_END);
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


	KeySet * ks = createSpec ();
	Key * errorKey = keyNew (BASE_KEY, KEY_END);

	int result = elektraGetOpts (ks, argc, argv, (const char **) environ, errorKey);
	if (result == -1)
	{
		// there was an error
		fprintf (stderr, "ERROR: %s\n", keyString (keyGetMeta (errorKey, "error/reason")));
		keyDel (errorKey);
		ksDel (ks);
		return EXIT_FAILURE;
	}

	if (result == 1)
	{
		// '--help' option was used
		char * help = elektraGetOptsHelpMessage (errorKey, NULL, NULL);
		fprintf (stderr, "%s\n", help);
		elektraFree (help);
		keyDel (errorKey);
		ksDel (ks);
		return EXIT_SUCCESS;
	}

	printf ("A real implementation would now\n");

	Key * lookup = ksLookupByName (ks, BASE_KEY "/printversion", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
	{
		printf ("print version information\n");

		keyDel (errorKey);
		ksDel (ks);

		return EXIT_SUCCESS;
	}

	lookup = ksLookupByName (ks, BASE_KEY "", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "getter") == 0)
	{
		lookup = ksLookupByName (ks, BASE_KEY "/getter/keyname", 0);
		if (lookup == NULL || strlen (keyString (lookup)) == 0)
		{
			printf ("report the error 'empty parameter: keyname'\n");
			keyDel (errorKey);
			ksDel (ks);

			return EXIT_SUCCESS;
		}
		printf ("get the key '%s'\n", keyString (lookup));

		lookup = ksLookupByName (ks, BASE_KEY "/getter/verbose", 0);
		if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
		{
			printf ("print where the read key value comes from\n");
		}
	}
	else if (lookup != NULL && elektraStrCmp (keyString (lookup), "setter") == 0)
	{
		const char * keyname;

		lookup = ksLookupByName (ks, BASE_KEY "/setter/keyname", 0);
		if (lookup == NULL || strlen (keyname = keyString (lookup)) == 0)
		{
			printf ("report the error 'missing parameter: keyname'\n");
			keyDel (errorKey);
			ksDel (ks);

			return EXIT_SUCCESS;
		}
		lookup = ksLookupByName (ks, BASE_KEY "/setter/value", 0);
		if (lookup == NULL)
		{
			printf ("report the error 'missing parameter: value'\n");
			keyDel (errorKey);
			ksDel (ks);

			return EXIT_SUCCESS;
		}
		printf ("set the key '%s' with the value '%s'\n", keyname, keyString (lookup));

		lookup = ksLookupByName (ks, BASE_KEY "/setter/verbose", 0);
		if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
		{
			printf ("print where the key value is stored now\n");
		}
	}
	else
	{
		Key * arrayParent = ksLookupByName (ks, BASE_KEY "/dynamic", 0);
		KeySet * dynamicCommand = elektraArrayGet (arrayParent, ks);

		if (ksGetSize (dynamicCommand) > 0)
		{
			printf ("dynamically invoke the command '");
			printf ("%s' with arguments:", keyString (ksAtCursor (dynamicCommand, 0)));
			Key * cur = NULL;

			for (elektraCursor it = 1; it < ksGetSize (dynamicCommand); ++it)
			{
				cur = ksAtCursor (dynamicCommand, it);
				printf (" %s", keyString (cur));
			}
			printf ("\n");
		}
		else
		{
			printf ("do nothing\n");
		}
	}

	keyDel (errorKey);
	ksDel (ks);

	return EXIT_SUCCESS;
}

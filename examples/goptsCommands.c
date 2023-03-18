/**
 * @file
 *
 * @brief Example for using command-line options with sub-commands
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/ease/old_ease.h>
#include <elektra/kdb.h>
#include <elektra/kdb/contracts/gopts.h>
#include <internal/utility/old_helper.h>

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

static int setupSpec (void)
{
	Key * parentKey = keyNew (SPEC_BASE_KEY, KEY_END);
	KDB * kdb = kdbOpen (NULL, parentKey);
	KeySet * ks = ksNew (0, KS_END);
	kdbGet (kdb, ks, parentKey);

	KeySet * existing = ksCut (ks, parentKey);
	if (ksGetSize (existing) > 0)
	{
		kdbClose (kdb, parentKey);
		ksDel (ks);
		ksDel (existing);
		return 0;
	}
	ksDel (existing);

	KeySet * spec = createSpec ();
	ksAppend (ks, spec);
	ksDel (spec);
	kdbSet (kdb, ks, parentKey);
	kdbClose (kdb, parentKey);
	ksDel (ks);

	return 1;
}


static void removeSpec (void)
{
	Key * parentKey = keyNew (SPEC_BASE_KEY, KEY_END);
	KDB * kdb = kdbOpen (NULL, parentKey);
	KeySet * ks = ksNew (0, KS_END);
	kdbGet (kdb, ks, parentKey);
	KeySet * spec = ksCut (ks, parentKey);
	ksDel (spec);
	kdbSet (kdb, ks, parentKey);
	kdbClose (kdb, parentKey);
	ksDel (ks);
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

	Key * parentKey = keyNew (BASE_KEY, KEY_END);
	KeySet * goptsConfig = ksNew (0, KS_END);
	KeySet * contract = ksNew (0, KS_END);

	elektraGOptsContract (contract, argc, argv, (const char * const *) environ, parentKey, goptsConfig);

	KDB * kdb = kdbOpen (contract, parentKey);

	KeySet * ks = ksNew (0, KS_END);
	int rc = kdbGet (kdb, ks, parentKey);

	if (rc == -1)
	{
		fprintf (stderr, "ERROR: kdbGet failed! %s\n", keyString (keyGetMeta (parentKey, "error/reason")));
		kdbClose (kdb, parentKey);
		keyDel (parentKey);
		ksDel (ks);
		removeSpec ();
		return EXIT_FAILURE;
	}

	Key * helpKey = ksLookupByName (ks, "proc:/elektra/gopts/help", 0);
	if (helpKey != NULL && elektraStrCmp (keyString (helpKey), "1") == 0)
	{
		const char * help = keyString (ksLookupByName (ks, "proc:/elektra/gopts/help/message", 0));
		printf ("%s\n", help);
		kdbClose (kdb, parentKey);
		keyDel (parentKey);
		ksDel (ks);
		removeSpec ();
		return EXIT_SUCCESS;
	}

	printf ("A real implementation would now\n");

	Key * lookup = ksLookupByName (ks, BASE_KEY "/printversion", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
	{
		printf ("print version information\n");

		kdbClose (kdb, parentKey);
		keyDel (parentKey);
		ksDel (ks);
		removeSpec ();
		return EXIT_SUCCESS;
	}

	lookup = ksLookupByName (ks, BASE_KEY "", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "getter") == 0)
	{
		lookup = ksLookupByName (ks, BASE_KEY "/getter/keyname", 0);
		if (lookup == NULL || strlen (keyString (lookup)) == 0)
		{
			printf ("report the error 'empty parameter: keyname'\n");
			kdbClose (kdb, parentKey);
			keyDel (parentKey);
			ksDel (ks);
			removeSpec ();
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
			kdbClose (kdb, parentKey);
			keyDel (parentKey);
			ksDel (ks);
			removeSpec ();
			return EXIT_SUCCESS;
		}
		lookup = ksLookupByName (ks, BASE_KEY "/setter/value", 0);
		if (lookup == NULL)
		{
			printf ("report the error 'missing parameter: value'\n");
			kdbClose (kdb, parentKey);
			keyDel (parentKey);
			ksDel (ks);
			removeSpec ();
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

			for (elektraCursor it = 1; it < ksGetSize (dynamicCommand); ++it)
			{
				Key * cur = ksAtCursor (dynamicCommand, it);
				printf (" %s", keyString (cur));
			}
			printf ("\n");
		}
		else
		{
			printf ("do nothing\n");
		}
	}

	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	ksDel (ks);

	// normally, you shouldn't remove the spec,
	// because you shouldn't have mounted it inside the application,
	// we do this just to keep the example self-contained
	removeSpec ();

	return EXIT_SUCCESS;
}

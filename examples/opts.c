/**
 * @file
 *
 * @brief Advanced use example for elektraGetOpts
 *        You should prefer the example in gopts.c over this one.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/ease/old_ease.h>
#include <internal/utility/old_helper.h>
#include <elektra/opts.h>

#include <stdio.h>
#include <stdlib.h>

extern char ** environ;

#define BASE_KEY "/sw/org/erm/#0/current"
#define SPEC_BASE_KEY "spec:" BASE_KEY

static KeySet * createSpec (void)
{
	return ksNew (
		10,
		keyNew (SPEC_BASE_KEY "/emptydirs", KEY_META, "description", "remove empty directories", KEY_META, "opt", "d", KEY_META,
			"opt/arg", "none", KEY_META, "opt/long", "dir", KEY_END),
		keyNew (SPEC_BASE_KEY "/force", KEY_META, "description", "ignore nonexistent files and arguments, never prompt", KEY_META,
			"opt", "f", KEY_META, "opt/arg", "none", KEY_META, "opt/long", "force", KEY_END),
		keyNew (SPEC_BASE_KEY "/interactive", KEY_META, "description",
			"prompt according to WHEN: never, once (-I), or always (-i), without WHEN, prompt always", KEY_META, "opt", "#1",
			KEY_META, "opt/#0", "i", KEY_META, "opt/#0/arg", "optional", KEY_META, "opt/#0/flagvalue", "always", KEY_META,
			"opt/#0/long", "interactive", KEY_META, "opt/#1", "I", KEY_META, "opt/#1/arg", "none", KEY_META, "opt/#1/flagvalue",
			"once", KEY_META, "opt/#0/arg/help", "WHEN", KEY_END),
		keyNew (SPEC_BASE_KEY "/nopreserve", KEY_META, "description", "do not treat '/' specially", KEY_META, "opt/arg", "none",
			KEY_META, "opt/long", "no-preserve-root", KEY_END),
		keyNew (SPEC_BASE_KEY "/preserve", KEY_META, "description",
			"do not remove '/' (default), with 'all', reject any command line argument on a separate device from its parent",
			KEY_META, "opt/arg", "optional", KEY_META, "opt/arg/help", "all", KEY_META, "opt/flagvalue", "root", KEY_META,
			"opt/long", "preserve-root", KEY_END),
		keyNew (SPEC_BASE_KEY "/recursive", KEY_META, "description", "remove directories and their contents recursively", KEY_META,
			"opt", "#1", KEY_META, "opt/#0", "r", KEY_META, "opt/#0/arg", "none", KEY_META, "opt/#0/long", "recursive",
			KEY_META, "opt/#1", "R", KEY_META, "opt/#1/arg", "none", KEY_END),
		keyNew (SPEC_BASE_KEY "/showversion", KEY_META, "description", "output version information and exit", KEY_META, "opt/arg",
			"none", KEY_META, "opt/long", "version", KEY_END),
		keyNew (SPEC_BASE_KEY "/singlefs", KEY_META, "description",
			"when removing a hierarchy recursively, skip any directory that is on a file system different from that of the "
			"corresponding line argument",
			KEY_META, "opt/arg", "none", KEY_META, "opt/long", "one-file-system", KEY_END),
		keyNew (SPEC_BASE_KEY "/verbose", KEY_META, "description", "explain what is being done", KEY_META, "opt", "v", KEY_META,
			"opt/arg", "none", KEY_META, "opt/long", "verbose", KEY_META, "env", "VERBOSE", KEY_END),
		keyNew (SPEC_BASE_KEY "/files/#", KEY_META, "description", "the files that shall be deleted", KEY_META, "args", "remaining",
			KEY_META, "env", "FILES", KEY_END),
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
		fprintf (stderr, "ERROR: %s\n", keyString (keyGetMeta (errorKey, "error/reason")));
		keyDel (errorKey);
		ksDel (ks);
		return EXIT_FAILURE;
	}

	if (result == 1)
	{
		char * help = elektraGetOptsHelpMessage (errorKey, NULL, NULL);
		fprintf (stderr, "%s\n", help);
		elektraFree (help);
		keyDel (errorKey);
		ksDel (ks);
		return EXIT_SUCCESS;
	}

	printf ("When called with the same arguments 'rm' \n");

	Key * lookup = ksLookupByName (ks, BASE_KEY "/emptydirs", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
	{
		printf ("will delete empty directories\n");
	}

	lookup = ksLookupByName (ks, BASE_KEY "/force", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
	{
		printf ("will use force mode\n");
	}

	lookup = ksLookupByName (ks, BASE_KEY "/interactive", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "never") == 0)
	{
		printf ("will not use interactive mode\n");
	}
	else if (lookup != NULL && elektraStrCmp (keyString (lookup), "once") == 0)
	{
		printf ("will use interactive mode; ask once\n");
	}
	else if (lookup != NULL && elektraStrCmp (keyString (lookup), "always") == 0)
	{
		printf ("will use interactive mode; always ask\n");
	}

	lookup = ksLookupByName (ks, BASE_KEY "/nopreserve", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
	{
		printf ("will not treat '/' specially\n");
	}

	lookup = ksLookupByName (ks, BASE_KEY "/preserve", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "root") == 0)
	{
		printf ("will never remove '/'");
	}
	else if (lookup != NULL && elektraStrCmp (keyString (lookup), "all") == 0)
	{
		printf ("will reject any argument on separate device from its parent\n");
	}

	lookup = ksLookupByName (ks, BASE_KEY "/recursive", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
	{
		printf ("will delete recursively\n");
	}

	lookup = ksLookupByName (ks, BASE_KEY "/showversion", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
	{
		printf ("will show version and exit\n");
	}

	lookup = ksLookupByName (ks, BASE_KEY "/singlefs", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
	{
		printf ("will skip directories on different file systems\n");
	}

	lookup = ksLookupByName (ks, BASE_KEY "/verbose", 0);
	if (lookup != NULL && elektraStrCmp (keyString (lookup), "1") == 0)
	{
		printf ("will explain what is being done\n");
	}

	printf ("will remove the following files:\n");

	Key * arrayParent = ksLookupByName (ks, BASE_KEY "/files", 0);
	KeySet * files = elektraArrayGet (arrayParent, ks);
	Key * cur = NULL;

	for (elektraCursor it = 0; it < ksGetSize (files); ++it)
	{
		cur = ksAtCursor (files, it);
		printf ("  %s\n", keyString (cur));
	}
	printf ("\n");

	keyDel (errorKey);
	ksDel (ks);

	return EXIT_SUCCESS;
}

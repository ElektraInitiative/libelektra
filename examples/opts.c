/**
 * @file
 *
 * @brief Advanced use example for elektraGetOpts
 *        You should prefer the example in gopts.c over this one.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbease.h>
#include <kdbhelper.h>
#include <kdbopts.h>

#include <stdio.h>
#include <stdlib.h>

extern char ** environ;

#define BASE_KEY "/sw/org/erm/#0/current"
#define SPEC_BASE_KEY "spec:" BASE_KEY

static ElektraKeyset * createSpec (void)
{
	return elektraKeysetNew (
		10,
		elektraKeyNew (SPEC_BASE_KEY "/emptydirs", ELEKTRA_KEY_META, "description", "remove empty directories", ELEKTRA_KEY_META, "opt", "d", ELEKTRA_KEY_META,
			"opt/arg", "none", ELEKTRA_KEY_META, "opt/long", "dir", ELEKTRA_KEY_END),
		elektraKeyNew (SPEC_BASE_KEY "/force", ELEKTRA_KEY_META, "description", "ignore nonexistent files and arguments, never prompt", ELEKTRA_KEY_META,
			"opt", "f", ELEKTRA_KEY_META, "opt/arg", "none", ELEKTRA_KEY_META, "opt/long", "force", ELEKTRA_KEY_END),
		elektraKeyNew (SPEC_BASE_KEY "/interactive", ELEKTRA_KEY_META, "description",
			"prompt according to WHEN: never, once (-I), or always (-i), without WHEN, prompt always", ELEKTRA_KEY_META, "opt", "#1",
			ELEKTRA_KEY_META, "opt/#0", "i", ELEKTRA_KEY_META, "opt/#0/arg", "optional", ELEKTRA_KEY_META, "opt/#0/flagvalue", "always", ELEKTRA_KEY_META,
			"opt/#0/long", "interactive", ELEKTRA_KEY_META, "opt/#1", "I", ELEKTRA_KEY_META, "opt/#1/arg", "none", ELEKTRA_KEY_META, "opt/#1/flagvalue",
			"once", ELEKTRA_KEY_META, "opt/arg/name", "WHEN", ELEKTRA_KEY_END),
		elektraKeyNew (SPEC_BASE_KEY "/nopreserve", ELEKTRA_KEY_META, "description", "do not treat '/' specially", ELEKTRA_KEY_META, "opt/arg", "none",
			ELEKTRA_KEY_META, "opt/long", "no-preserve-root", ELEKTRA_KEY_END),
		elektraKeyNew (SPEC_BASE_KEY "/preserve", ELEKTRA_KEY_META, "description",
			"do not remove '/' (default), with 'all', reject any command line argument on a separate device from its parent",
			ELEKTRA_KEY_META, "opt/arg", "optional", ELEKTRA_KEY_META, "opt/arg/name", "all", ELEKTRA_KEY_META, "opt/flagvalue", "root", ELEKTRA_KEY_META,
			"opt/long", "preserve-root", ELEKTRA_KEY_END),
		elektraKeyNew (SPEC_BASE_KEY "/recursive", ELEKTRA_KEY_META, "description", "remove directories and their contents recursively", ELEKTRA_KEY_META,
			"opt", "#1", ELEKTRA_KEY_META, "opt/#0", "r", ELEKTRA_KEY_META, "opt/#0/arg", "none", ELEKTRA_KEY_META, "opt/#0/long", "recursive",
			ELEKTRA_KEY_META, "opt/#1", "R", ELEKTRA_KEY_META, "opt/#1/arg", "none", ELEKTRA_KEY_END),
		elektraKeyNew (SPEC_BASE_KEY "/showversion", ELEKTRA_KEY_META, "description", "output version information and exit", ELEKTRA_KEY_META, "opt/arg",
			"none", ELEKTRA_KEY_META, "opt/long", "version", ELEKTRA_KEY_END),
		elektraKeyNew (SPEC_BASE_KEY "/singlefs", ELEKTRA_KEY_META, "description",
			"when removing a hierarchy recursively, skip any directory that is on a file system different from that of the "
			"corresponding line argument",
			ELEKTRA_KEY_META, "opt/arg", "none", ELEKTRA_KEY_META, "opt/long", "one-file-system", ELEKTRA_KEY_END),
		elektraKeyNew (SPEC_BASE_KEY "/verbose", ELEKTRA_KEY_META, "description", "explain what is being done", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META,
			"opt/arg", "none", ELEKTRA_KEY_META, "opt/long", "verbose", ELEKTRA_KEY_META, "env", "VERBOSE", ELEKTRA_KEY_END),
		elektraKeyNew (SPEC_BASE_KEY "/files/#", ELEKTRA_KEY_META, "description", "the files that shall be deleted", ELEKTRA_KEY_META, "args", "remaining",
			ELEKTRA_KEY_META, "env", "FILES", ELEKTRA_KEY_END),
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
		fprintf (stderr, "ERROR: %s\n", elektraKeyString (elektraKeyGetMeta (errorKey, "error/reason")));
		elektraKeyDel (errorKey);
		elektraKeysetDel (ks);
		return EXIT_FAILURE;
	}

	if (result == 1)
	{
		char * help = elektraGetOptsHelpMessage (errorKey, NULL, NULL);
		fprintf (stderr, "%s\n", help);
		elektraFree (help);
		elektraKeyDel (errorKey);
		elektraKeysetDel (ks);
		return EXIT_SUCCESS;
	}

	printf ("When called with the same arguments 'rm' \n");

	ElektraKey * lookup = elektraKeysetLookupByName (ks, BASE_KEY "/emptydirs", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
	{
		printf ("will delete empty directories\n");
	}

	lookup = elektraKeysetLookupByName (ks, BASE_KEY "/force", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
	{
		printf ("will use force mode\n");
	}

	lookup = elektraKeysetLookupByName (ks, BASE_KEY "/interactive", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "never") == 0)
	{
		printf ("will not use interactive mode\n");
	}
	else if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "once") == 0)
	{
		printf ("will use interactive mode; ask once\n");
	}
	else if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "always") == 0)
	{
		printf ("will use interactive mode; always ask\n");
	}

	lookup = elektraKeysetLookupByName (ks, BASE_KEY "/nopreserve", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
	{
		printf ("will not treat '/' specially\n");
	}

	lookup = elektraKeysetLookupByName (ks, BASE_KEY "/preserve", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "root") == 0)
	{
		printf ("will never remove '/'");
	}
	else if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "all") == 0)
	{
		printf ("will reject any argument on separate device from its parent\n");
	}

	lookup = elektraKeysetLookupByName (ks, BASE_KEY "/recursive", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
	{
		printf ("will delete recursively\n");
	}

	lookup = elektraKeysetLookupByName (ks, BASE_KEY "/showversion", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
	{
		printf ("will show version and exit\n");
	}

	lookup = elektraKeysetLookupByName (ks, BASE_KEY "/singlefs", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
	{
		printf ("will skip directories on different file systems\n");
	}

	lookup = elektraKeysetLookupByName (ks, BASE_KEY "/verbose", 0);
	if (lookup != NULL && elektraStrCmp (elektraKeyString (lookup), "1") == 0)
	{
		printf ("will explain what is being done\n");
	}

	printf ("will remove the following files:\n");

	ElektraKey * arrayParent = elektraKeysetLookupByName (ks, BASE_KEY "/files", 0);
	ElektraKeyset * files = elektraArrayGet (arrayParent, ks);

	elektraKeysetRewind (files);
	ElektraKey * cur = NULL;
	while ((cur = elektraKeysetNext (files)) != NULL)
	{
		printf ("  %s\n", elektraKeyString (cur));
	}
	printf ("\n");

	elektraKeyDel (errorKey);
	elektraKeysetDel (ks);

	return EXIT_SUCCESS;
}

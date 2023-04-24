/**
 * @file
 *
 * @brief Example for using command-line options
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/core/key.h>
#include <elektra/core/keyset.h>
#include <elektra/ease/array.h>
#include <elektra/kdb/contracts/gopts.h>
#include <elektra/kdb/kdb.h>
#include <internal/utility/old_helper.h>

#include <stdio.h>
#include <stdlib.h>

extern char ** environ;

#define BASE_KEY "/sw/org/erm/#0/current"
#define SPEC_BASE_KEY "spec:" BASE_KEY

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

int main (int argc, const char * const * argv)
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

	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	ksDel (ks);

	// normally, you shouldn't remove the spec,
	// because you shouldn't have mounted it inside the application,
	// we do this just to keep the example self-contained
	removeSpec ();

	return EXIT_SUCCESS;
}

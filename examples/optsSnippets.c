/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbhelper.h>
#include <kdbopts.h>

#include <stdio.h>
#include <stdlib.h>

extern char ** environ;

static int basicUse (int argc, const char ** argv)
{
	Key * parentKey = keyNew ("/sw/org/example/#0/current", KEY_END);
	//! [basic use]
	KDB * kdb = kdbOpen (parentKey);
	KeySet * ks = ksNew (0, KS_END);

	kdbGet (kdb, ks, parentKey);

	// cut out our part of the kdb
	KeySet * spec = ksCut (ks, parentKey);

	int result = elektraGetOpts (spec, argc, argv, (const char **) environ, parentKey);
	if (result == -1)
	{
		fprintf (stderr, "ERROR: %s\n", keyString (keyGetMeta (parentKey, "error/reason")));
		keyDel (parentKey);
		ksDel (ks);
		return EXIT_FAILURE;
	}

	if (result == 1)
	{
		char * help = elektraGetOptsHelpMessage (parentKey, NULL, NULL);
		fprintf (stderr, "%s\n", help);
		elektraFree (help);
		keyDel (parentKey);
		ksDel (ks);
		return EXIT_SUCCESS;
	}

	// merge the results back into the kdb
	ksAppend (ks, spec);
	ksDel (spec);

	//! [basic use]
	ksDel (ks);
	kdbClose (kdb, parentKey);
	keyDel (parentKey);
	return EXIT_SUCCESS;
}

int main (int argc, const char ** argv)
{
	return basicUse (argc, argv);
}

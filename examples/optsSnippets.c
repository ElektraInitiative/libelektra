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
	KDB * kdb = kdbOpen (NULL, parentKey);
	KeySet * ks = ksNew (0, KS_END);

	kdbGet (kdb, ks, parentKey);

	int result = elektraGetOpts (ks, argc, argv, (const char **) environ, parentKey);
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

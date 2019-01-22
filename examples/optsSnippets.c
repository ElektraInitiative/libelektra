/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbopts.h>

#include <stdio.h>
#include <stdlib.h>

extern char ** environ;

int basicUse (int argc, const char ** argv)
{
	Key * parentKey = keyNew ("");
	//! [basic use]
	KDB * kdb = kdbOpen (parentKey);
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
}

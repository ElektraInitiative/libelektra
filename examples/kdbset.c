/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdb.h>
#include <kdbmerge.h>

#include <stdio.h>
#include <stdlib.h>

typedef enum
{
	INPUT_USE_OURS,
	INPUT_DO_MERGE,
	INPUT_USE_THEIRS
} input;

int showElektraErrorDialog (ElektraKey * parentKey)
{
	printf ("dialog for %s\n", elektraKeyName (parentKey));
	int a;
	if (scanf ("%d", &a) != 1)
	{
		fprintf (stderr, "Unable to convert input to integer number");
		return EXIT_FAILURE;
	}
	return a;
}

ElektraKeyset * doElektraMerge (ElektraKeyset * ours, ElektraKeyset * theirs, ElektraKeyset * base)
{
	printf ("see libelektra-tools for merging"
		" sizes are: %d %d %d\n",
		(int) elektraKeysetGetSize (ours), (int) elektraKeysetGetSize (theirs), (int) elektraKeysetGetSize (base));
	return elektraKeysetNew (0, ELEKTRA_KS_END);
}


int main (void)
{
	// clang-format off
//! [set]
ElektraKeyset * myConfig = elektraKeysetNew (0, ELEKTRA_KS_END);
ElektraKey * parentKey = elektraKeyNew ("system:/sw/MyApp", ELEKTRA_KEY_END);
ElektraKdb * handle = elektraKdbOpen (NULL, parentKey);

elektraKdbGet (handle, myConfig, parentKey); // kdbGet needs to be called first!
ElektraKeyset * base = elektraKeysetDup (myConfig);     // save a copy of original keyset

// change the keys within myConfig
elektraKeysetAppendKey (myConfig, elektraKeyNew ("system:/sw/MyApp/Test", ELEKTRA_KEY_VALUE, "5", ELEKTRA_KEY_END));

ElektraKeyset * ours = elektraKeysetDup (myConfig); // save a copy of our keyset
ElektraKeyset * theirs;		  // needed for 3-way merging
int ret = elektraKdbSet (handle, myConfig, parentKey);
while (ret == -1) // as long as we have an error
{
	int strategy = showElektraErrorDialog (parentKey);
	theirs = elektraKeysetDup (ours);
	elektraKdbGet (handle, theirs, parentKey); // refresh key database
	ElektraKeyset * result = elektraMerge(
		elektraKeysetCut(ours, parentKey), parentKey,
		elektraKeysetCut(theirs, parentKey), parentKey,
		elektraKeysetCut(base, parentKey), parentKey,
		parentKey, strategy, parentKey);
	int numberOfConflicts = getConflicts (parentKey);
	elektraKeysetDel (theirs);
	if (result != NULL) {
		ret = elektraKdbSet (handle, result, parentKey);
	} else {
		// an error happened while merging
		if (numberOfConflicts > 0 && strategy == MERGE_STRATEGY_ABORT)
		{
			// Error due to merge conflicts
			ret = -1;
		}
		else
		{
			// Internal errors, out of memory etc.
			ret = -1;
		}
	}
}


elektraKeysetDel (ours);
elektraKeysetDel (base);
elektraKeysetDel (myConfig); // delete the in-memory configuration

elektraKdbClose (handle, parentKey); // no more affairs with the key database.
elektraKeyDel (parentKey);
//! [set]
}

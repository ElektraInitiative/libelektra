/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/merge.h>
#include <elektra/old_kdb.h>

#include <stdio.h>
#include <stdlib.h>

typedef enum
{
	INPUT_USE_OURS,
	INPUT_DO_MERGE,
	INPUT_USE_THEIRS
} input;

int showElektraErrorDialog (Key * parentKey)
{
	printf ("dialog for %s\n", keyName (parentKey));
	int a;
	if (scanf ("%d", &a) != 1)
	{
		fprintf (stderr, "Unable to convert input to integer number");
		return EXIT_FAILURE;
	}
	return a;
}

KeySet * doElektraMerge (KeySet * ours, KeySet * theirs, KeySet * base)
{
	printf ("see libelektra-tools for merging"
		" sizes are: %d %d %d\n",
		(int) ksGetSize (ours), (int) ksGetSize (theirs), (int) ksGetSize (base));
	return ksNew (0, KS_END);
}


int main (void)
{
	// clang-format off
//! [set]
KeySet * myConfig = ksNew (0, KS_END);
Key * parentKey = keyNew ("system:/sw/MyApp", KEY_END);
KDB * handle = kdbOpen (NULL, parentKey);

kdbGet (handle, myConfig, parentKey); // kdbGet needs to be called first!
KeySet * base = ksDup (myConfig);     // save a copy of original keyset

// change the keys within myConfig
ksAppendKey (myConfig, keyNew ("system:/sw/MyApp/Test", KEY_VALUE, "5", KEY_END));

KeySet * ours = ksDup (myConfig); // save a copy of our keyset
KeySet * theirs;		  // needed for 3-way merging
int ret = kdbSet (handle, myConfig, parentKey);
while (ret == -1) // as long as we have an error
{
	int strategy = showElektraErrorDialog (parentKey);
	theirs = ksDup (ours);
	kdbGet (handle, theirs, parentKey); // refresh key database
	KeySet * result = elektraMerge(
		ksCut(ours, parentKey), parentKey,
		ksCut(theirs, parentKey), parentKey,
		ksCut(base, parentKey), parentKey,
		parentKey, strategy, parentKey);
	int numberOfConflicts = elektraMergeGetConflicts (parentKey);
	ksDel (theirs);
	if (result != NULL) {
		ret = kdbSet (handle, result, parentKey);
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


ksDel (ours);
ksDel (base);
ksDel (myConfig); // delete the in-memory configuration

kdbClose (handle, parentKey); // no more affairs with the key database.
keyDel (parentKey);
//! [set]
}

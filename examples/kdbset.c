/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb.h>

#include <stdio.h>
#include <stdlib.h>

typedef enum
{
	INPUT_USE_OURS,
	INPUT_DO_MERGE,
	INPUT_USE_THEIRS
} input;

int showElektraErrorDialog (Key * parentKey, Key * problemKey)
{
	printf ("dialog for %s and %s\n", keyName (parentKey), keyName (problemKey));
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
Key * parentKey = keyNew ("system/sw/MyApp", KEY_END);
KDB * handle = kdbOpen (parentKey);

kdbGet (handle, myConfig, parentKey); // kdbGet needs to be called first!
KeySet * base = ksDup (myConfig);     // save a copy of original keyset

// change the keys within myConfig

KeySet * ours = ksDup (myConfig); // save a copy of our keyset
KeySet * theirs;		  // needed for 3-way merging
int ret = kdbSet (handle, myConfig, parentKey);
while (ret == -1) // as long as we have an error
{
	// We got an error. Warn user.
	Key * problemKey = ksCurrent (myConfig);
	// parentKey has the errorInformation
	// problemKey is the faulty key (may be null)
	int userInput = showElektraErrorDialog (parentKey, problemKey);
	switch (userInput)
	{
	case INPUT_USE_OURS:
		kdbGet (handle, myConfig, parentKey); // refresh key database
		ksDel (myConfig);
		myConfig = ours;
		break;
	case INPUT_DO_MERGE:
		theirs = ksDup (ours);
		kdbGet (handle, theirs, parentKey); // refresh key database
		KeySet * res = doElektraMerge (ours, theirs, base);
		ksDel (theirs);
		myConfig = res;
		break;
	case INPUT_USE_THEIRS:
		// should always work, we just write what we got
		// but to be sure always give the user another way
		// to exit the loop
		kdbGet (handle, myConfig, parentKey); // refresh key database
		break;
		// other cases ...
	}
	ret = kdbSet (handle, myConfig, parentKey);
}

ksDel (ours);
ksDel (base);
ksDel (myConfig); // delete the in-memory configuration

kdbClose (handle, parentKey); // no more affairs with the key database.
keyDel (parentKey);
//! [set]
}

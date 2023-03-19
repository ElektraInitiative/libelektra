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

static void print_results (KeySet * result, Key * resultRoot, Key * informationKey)
{
	KeySet * conflictingKeys = elektraMergeGetConflictingKeys (informationKey, resultRoot);

	for (elektraCursor i = 0; i < ksGetSize (conflictingKeys); i++)
	{
		printf ("Conflict in key %s\n", keyName (ksAtCursor (conflictingKeys, i)));
	}

	printf ("Result:\n");

	for (elektraCursor i = 0; i < ksGetSize (result); i++)
	{
		Key * k = ksAtCursor (result, i);
		printf ("%s = %s\n", keyName (k), keyString (k));
	}

	if (result == NULL)
	{
		printf ("Aborted.\n");
	}

	ksDel (conflictingKeys);
}

int main (void)
{
	Key * baseRoot = keyNew ("user:/screen", KEY_END);
	KeySet * base = ksNew (1, keyNew ("user:/screen/background", KEY_VALUE, "blue", KEY_END), KS_END);

	Key * theirRoot = keyDup (baseRoot, KEY_CP_ALL);
	KeySet * their = ksNew (2, keyNew ("user:/screen/background", KEY_VALUE, "red", KEY_END),
				keyNew ("user:/screen/brightness", KEY_VALUE, "100", KEY_END), KS_END);

	Key * ourRoot = keyDup (baseRoot, KEY_CP_ALL);
	KeySet * our = ksNew (2, keyNew ("user:/screen/background", KEY_VALUE, "purple", KEY_END),
			      keyNew ("user:/screen/standby", KEY_VALUE, "on", KEY_END), KS_END);

	Key * resultRoot = keyDup (baseRoot, KEY_CP_ALL);
	Key * informationKey = keyDup (baseRoot, KEY_CP_ALL);

	// Strategy: Abort
	// Will abort due to user:/screen/background changes in both their and our
	printf ("Trying merge strategy ABORT\n");
	KeySet * result = elektraMerge (our, ourRoot, their, theirRoot, base, baseRoot, resultRoot, MERGE_STRATEGY_ABORT, informationKey);
	print_results (result, resultRoot, informationKey);

	ksDel (result);

	// Strategy: Our
	// Will take value of our for user:/screen/background
	printf ("\nTrying merge strategy OUR\n");
	result = elektraMerge (our, ourRoot, their, theirRoot, base, baseRoot, resultRoot, MERGE_STRATEGY_OUR, informationKey);
	print_results (result, resultRoot, informationKey);

	ksDel (result);

	// Strategy: Their
	// Will take value of their for user:/screen/background
	printf ("\nTrying merge strategy THEIR\n");
	result = elektraMerge (our, ourRoot, their, theirRoot, base, baseRoot, resultRoot, MERGE_STRATEGY_THEIR, informationKey);
	print_results (result, resultRoot, informationKey);

	ksDel (result);

	ksDel (base);
	ksDel (their);
	ksDel (our);

	keyDel (baseRoot);
	keyDel (theirRoot);
	keyDel (ourRoot);
	keyDel (informationKey);
	keyDel (resultRoot);

	return 0;
}

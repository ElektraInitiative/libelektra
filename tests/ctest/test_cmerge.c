/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdbmerge.h>
#include <tests.h>

#define default_result_size 30 // Enough space for possible strange results
#define OUR_ROOT "user:/our"
#define THEIR_ROOT "user:/their"
#define BASE_ROOT "user:/base"
#define RESULT_ROOT "user:/result"
#define OUR_KEY1 "user:/our/key1"
#define THEIR_KEY1 "user:/their/key1"
#define BASE_KEY1 "user:/base/key1"
#define RESULT_KEY1 "user:/result/key1"
#define ORIGINAL_VALUE "1"
#define CHANGED_VALUE "2"
#define MORE_CHANGED_VALUE "3"
#define COMMENT "comment"
#define SOME_COMMENT "some_comment"
#define OTHER_COMMENT "other_comment"
#define OTHER_COMMENT_LENGTH 13
// This is arbitrarily chosen.
// In test cases were no conflict should occur the strategy is irrelevant.
#define MERGE_STRATEGY_IRRELEVANT 1

/**
 * When there is a single key in each key set and all key names are equal
 *
 * The parameters are the values for the single key in each set
 *
 * If a parameter is null then no key is set (!= key with empty value)
 *
 * If expected_result = NULL then the merge result must be NULL, this is for error cases
 * If expected_result = "EMPTY" then the merge result must not contain the key (it is completely
 * empty as only one key could be in it)
 */
static void simple_test (char * our_value, char * their_value, char * base_value, int strategy, char * expected_result)
{
	printf ("Executing %s with our=%s their=%s base=%s, strategy=%d, expected_result=%s\n", __func__, our_value, their_value,
		base_value, strategy, expected_result);
	Key * our_root = keyNew ("user:/our", KEY_END);
	Key * their_root = keyNew ("user:/their", KEY_END);
	Key * base_root = keyNew ("user:/base", KEY_END);
	Key * result_root = keyNew ("user:/result", KEY_END);
	Key * informationKey = keyNew ("/", KEY_END);
	KeySet * our = ksNew (0, KS_END);
	KeySet * their = ksNew (0, KS_END);
	KeySet * base = ksNew (0, KS_END);
	if (strcmp (our_value, "EMPTY") != 0)
	{
		ksAppendKey (our, keyNew ("user:/our/key", KEY_VALUE, our_value, KEY_END));
	}
	if (strcmp (their_value, "EMPTY") != 0)
	{
		ksAppendKey (their, keyNew ("user:/their/key", KEY_VALUE, their_value, KEY_END));
	}
	if (strcmp (base_value, "EMPTY") != 0)
	{
		ksAppendKey (base, keyNew ("user:/base/key", KEY_VALUE, base_value, KEY_END));
	}
	KeySet * result = elektraMerge (our, our_root, their, their_root, base, base_root, result_root, strategy, informationKey);

	if (expected_result == NULL)
	{
		char msg[200];
		snprintf (msg, 200,
			  "Executing %s with our=%s their=%s base=%s and strategy %i. Expected the merge result to be NULL but it was "
			  "existant.",
			  __func__, our_value, their_value, base_value, strategy);
		succeed_if (result == NULL, msg);
	}
	else
	{
		Key * resultKey = ksLookupByName (result, "user:/result/key", 0);
		if (resultKey == NULL)
		{
			char msg[200];
			snprintf (msg, 200,
				  "Executing %s with our=%s their=%s base=%s and strategy %i. Expected result to be %s and not an empty "
				  "key set.\n",
				  __func__, our_value, their_value, base_value, strategy, expected_result);
			succeed_if (strcmp (expected_result, "EMPTY") == 0, msg);
			succeed_if_same_string (expected_result, "EMPTY");
		}
		else
		{
			char * resultValue = elektraMalloc (default_result_size);
			keyGetString (resultKey, resultValue, default_result_size);
			char msg[200];
			snprintf (msg, 200,
				  "Executing %s with our=%s their=%s base=%s and strategy %i. Expected result was %s but in reality it was "
				  "%s.\n",
				  __func__, our_value, their_value, base_value, strategy, expected_result, resultValue);
			succeed_if (strcmp (resultValue, expected_result) == 0, msg);
			elektraFree (resultValue);
		}
	}

	ksDel (our);
	ksDel (their);
	ksDel (base);
	ksDel (result);
	keyDel (our_root);
	keyDel (their_root);
	keyDel (base_root);
	keyDel (result_root);
	keyDel (informationKey);
}

/**
 * Use this when the result of the merge is the same for all strategies
 */
static void all_strategies_same_result (char * our_value, char * their_value, char * base_value, char * expected_result)
{
	simple_test (our_value, their_value, base_value, MERGE_STRATEGY_ABORT, expected_result);
	simple_test (our_value, their_value, base_value, MERGE_STRATEGY_OUR, expected_result);
	simple_test (our_value, their_value, base_value, MERGE_STRATEGY_THEIR, expected_result);
}

/**
 * Use this when the merge conflicts or overlaps
 * According to https://www.gnu.org/software/diffutils/manual/html_node/diff3-Merging.html
 */
static void all_strategies_conflict (char * our_value, char * their_value, char * base_value)
{
	printf ("In %s with our=%s and their=%s and base=%s\n", __func__, our_value, their_value, base_value);
	simple_test (our_value, their_value, base_value, MERGE_STRATEGY_ABORT, NULL);
	simple_test (our_value, their_value, base_value, MERGE_STRATEGY_OUR, our_value);
	simple_test (our_value, their_value, base_value, MERGE_STRATEGY_THEIR, their_value);
}

static void test_order (char * our_order, char * their_order, char * base_order, int strategy, char * expected_result)
{
	printf ("Executing %s with our=%s their=%s base=%s, strategy=%d, expected_result=%s\n", __func__, our_order, their_order,
		base_order, strategy, expected_result);
	Key * our_root = keyNew ("user:/our", KEY_END);
	Key * their_root = keyNew ("user:/their", KEY_END);
	Key * base_root = keyNew ("user:/base", KEY_END);
	Key * result_root = keyNew ("user:/result", KEY_END);
	Key * informationKey = keyNew ("/", KEY_END);
	KeySet * our = ksNew (1, keyNew ("user:/our/key", KEY_VALUE, "1", KEY_META, "order", our_order, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew ("user:/their/key", KEY_VALUE, "1", KEY_META, "order", their_order, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew ("user:/base/key", KEY_VALUE, "1", KEY_META, "order", base_order, KEY_END), KS_END);

	KeySet * result = elektraMerge (our, our_root, their, their_root, base, base_root, result_root, strategy, informationKey);

	if (expected_result == NULL)
	{
		yield_error ("expected_result parameter must not be null");
	}
	else
	{
		const Key * resultKey = ksLookupByName (result, "user:/result/key", 0);
		if (resultKey == NULL)
		{
			yield_error ("Lookup must succeed");
		}
		const Key * metaKey = keyGetMeta (resultKey, "order");
		if (metaKey == NULL)
		{
			yield_error ("Meta key must exist");
		}
		char * resultValue = elektraMalloc (default_result_size);
		keyGetString (metaKey, resultValue, default_result_size);
		char msg[200];
		if (resultValue == NULL)
		{
			yield_error ("resultValue must not be null");
		}
		if (expected_result == NULL)
		{
			yield_error ("expectedResult must not be null");
		}
		if (resultValue != NULL && expected_result != NULL)
		{
			snprintf (msg, 200,
				  "Executing %s with our=%s their=%s base=%s and strategy %i. Expected result was %s but in reality it was "
				  "%s.\n",
				  __func__, our_order, their_order, base_order, strategy, expected_result, resultValue);
			succeed_if (strcmp (resultValue, expected_result) == 0, msg);
		}
		elektraFree (resultValue);
	}

	ksDel (our);
	ksDel (their);
	ksDel (base);
	ksDel (result);
	keyDel (our_root);
	keyDel (their_root);
	keyDel (base_root);
	keyDel (result_root);
	keyDel (informationKey);
}

/**
 * Checks if adding a single line to an array gives one error as happens in regular diff algorithms
 * or as many errors as there are number of lines, which a trivial implementation does.
 *
 *       ours their base result
 *  /#0  0    0     a
 *  /#1  1    1     0
 *  /#2  2    2     1
 *  /#3  3    3     2
 *  /#4             3
 */
static void array_conflict_number_test (void)
{
	printf ("Executing %s\n", __func__);
	Key * our_root = keyNew ("user:/our", KEY_END);
	Key * their_root = keyNew ("user:/their", KEY_END);
	Key * base_root = keyNew ("user:/base", KEY_END);
	Key * result_root = keyNew ("user:/result", KEY_END);
	Key * informationKey = keyNew (0, KEY_END);
	KeySet * our = ksNew (5, keyNew ("user:/our/#0", KEY_VALUE, "a", KEY_END), keyNew ("user:/our/#1", KEY_VALUE, "0", KEY_END),
			      keyNew ("user:/our/#2", KEY_VALUE, "1", KEY_END), keyNew ("user:/our/#3", KEY_VALUE, "2", KEY_END),
			      keyNew ("user:/our/#4", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * their =
		ksNew (4, keyNew ("user:/their/#0", KEY_VALUE, "0", KEY_END), keyNew ("user:/their/#1", KEY_VALUE, "1", KEY_END),
		       keyNew ("user:/their/#2", KEY_VALUE, "2", KEY_END), keyNew ("user:/their/#3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * base =
		ksNew (4, keyNew ("user:/base/#0", KEY_VALUE, "0", KEY_END), keyNew ("user:/base/#1", KEY_VALUE, "1", KEY_END),
		       keyNew ("user:/base/#2", KEY_VALUE, "2", KEY_END), keyNew ("user:/base/#3", KEY_VALUE, "3", KEY_END), KS_END);
	KeySet * result =
		elektraMerge (our, our_root, their, their_root, base, base_root, result_root, MERGE_STRATEGY_ABORT, informationKey);

	ksDel (our);
	ksDel (their);
	ksDel (base);
	ksDel (result);
	keyDel (our_root);
	keyDel (their_root);
	keyDel (base_root);
	keyDel (result_root);
	keyDel (informationKey);
}

static void testValuesWithGivenLength (int size)
{
	printf ("Executing %s with size %d\n", __func__, size);
	Key * our_root = keyNew ("user:/tests/our", KEY_END);
	Key * their_root = keyNew ("user:/tests/their", KEY_END);
	Key * base_root = keyNew ("user:/tests/base", KEY_END);
	Key * result_root = keyNew ("user:/tests/result", KEY_END);
	Key * informationKey = keyNew (0, KEY_END);
	char * value = elektraCalloc (size);
	memset (value, 'a', size - 1); // leave the last element \0
	// clang-format off
	KeySet * our = ksNew (3,
		keyNew ("user:/tests/our/#0", KEY_VALUE, value, KEY_END),
		keyNew ("user:/tests/our/#1", KEY_VALUE, value, KEY_END),
		keyNew ("user:/tests/our/#2", KEY_VALUE, value, KEY_END),
		KS_END);
	KeySet * their = ksNew (3,
		keyNew ("user:/tests/their/#0", KEY_VALUE, value, KEY_END),
		keyNew ("user:/tests/their/#1", KEY_VALUE, value, KEY_END),
		keyNew ("user:/tests/their/#2", KEY_VALUE, value, KEY_END),
		KS_END);
	KeySet * base = ksNew (3,
		keyNew ("user:/tests/base/#0", KEY_VALUE, value, KEY_END),
		keyNew ("user:/tests/base/#1", KEY_VALUE, value, KEY_END),
		keyNew ("user:/tests/base/#2", KEY_VALUE, value, KEY_END),
		KS_END);
	// clang-format on
	elektraFree (value);
	KeySet * result =
		elektraMerge (our, our_root, their, their_root, base, base_root, result_root, MERGE_STRATEGY_ABORT, informationKey);

	ksDel (our);
	ksDel (their);
	ksDel (base);
	ksDel (result);
	keyDel (our_root);
	keyDel (their_root);
	keyDel (base_root);
	keyDel (result_root);
	keyDel (informationKey);
}

/** Tests if the libgit array merge works with various sizes
 *  This should especially test the memory allocations in getValuesAsArray()
 */
static void testArrayWithDifferentLengths (void)
{
	// allocationSize is 64 in getValuesAsArray() in kdbmerge.c
	// 14 already allocates around 1 MB
	int size = 64;
	for (int i = 1; i < 14; i++)
	{
		testValuesWithGivenLength (size - 5); // avoid errors where we are 1 or 2 off
		testValuesWithGivenLength (size - 1);
		testValuesWithGivenLength (size);
		testValuesWithGivenLength (size + 1);
		testValuesWithGivenLength (size + 5);
		size *= 2; // getValuesAsArray() doubles the size, too
	}
}

int main (int argc, char ** argv)
{
	printf ("CMERGE       TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	testArrayWithDifferentLengths ();
	all_strategies_same_result ("EMPTY", "EMPTY", "EMPTY", "EMPTY");
	all_strategies_conflict ("EMPTY", "EMPTY", "1");
	all_strategies_same_result ("EMPTY", "1", "EMPTY", "1");
	all_strategies_same_result ("EMPTY", "1", "1", "EMPTY");
	all_strategies_same_result ("1", "EMPTY", "EMPTY", "1");
	all_strategies_same_result ("1", "EMPTY", "1", "EMPTY");
	all_strategies_conflict ("1", "1", "EMPTY");
	all_strategies_same_result ("1", "1", "1", "1");
	all_strategies_conflict ("1", "1", "2");
	all_strategies_same_result ("1", "2", "1", "2");
	all_strategies_same_result ("1", "2", "2", "1");
	all_strategies_same_result ("2", "1", "1", "2");
	all_strategies_same_result ("2", "1", "2", "1");
	all_strategies_conflict ("2", "2", "1");
	all_strategies_conflict ("1", "2", "3");
	all_strategies_conflict ("1", "2", "EMPTY");
	all_strategies_conflict ("1", "EMPTY", "3");
	all_strategies_conflict ("EMPTY", "2", "3");
	test_order ("1", "1", "1", 1, "1");
	test_order ("2", "1", "1", 1, "2");
	array_conflict_number_test ();

	printf ("\ntest_merge RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

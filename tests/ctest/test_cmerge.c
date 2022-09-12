/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbmerge.h>
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
	ElektraKey * our_root = elektraKeyNew ("user:/our", ELEKTRA_KEY_END);
	ElektraKey * their_root = elektraKeyNew ("user:/their", ELEKTRA_KEY_END);
	ElektraKey * base_root = elektraKeyNew ("user:/base", ELEKTRA_KEY_END);
	ElektraKey * result_root = elektraKeyNew ("user:/result", ELEKTRA_KEY_END);
	ElektraKey * informationKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKeyset * our = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * their = elektraKeysetNew (0, ELEKTRA_KS_END);
	ElektraKeyset * base = elektraKeysetNew (0, ELEKTRA_KS_END);
	if (strcmp (our_value, "EMPTY") != 0)
	{
		elektraKeysetAppendKey (our, elektraKeyNew ("user:/our/key", ELEKTRA_KEY_VALUE, our_value, ELEKTRA_KEY_END));
	}
	if (strcmp (their_value, "EMPTY") != 0)
	{
		elektraKeysetAppendKey (their, elektraKeyNew ("user:/their/key", ELEKTRA_KEY_VALUE, their_value, ELEKTRA_KEY_END));
	}
	if (strcmp (base_value, "EMPTY") != 0)
	{
		elektraKeysetAppendKey (base, elektraKeyNew ("user:/base/key", ELEKTRA_KEY_VALUE, base_value, ELEKTRA_KEY_END));
	}
	ElektraKeyset * result = elektraMerge (our, our_root, their, their_root, base, base_root, result_root, strategy, informationKey);

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
		ElektraKey * resultKey = elektraKeysetLookupByName (result, "user:/result/key", 0);
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
			elektraKeyGetString (resultKey, resultValue, default_result_size);
			char msg[200];
			snprintf (msg, 200,
				  "Executing %s with our=%s their=%s base=%s and strategy %i. Expected result was %s but in reality it was "
				  "%s.\n",
				  __func__, our_value, their_value, base_value, strategy, expected_result, resultValue);
			succeed_if (strcmp (resultValue, expected_result) == 0, msg);
			elektraFree (resultValue);
		}
	}

	elektraKeysetDel (our);
	elektraKeysetDel (their);
	elektraKeysetDel (base);
	elektraKeysetDel (result);
	elektraKeyDel (our_root);
	elektraKeyDel (their_root);
	elektraKeyDel (base_root);
	elektraKeyDel (result_root);
	elektraKeyDel (informationKey);
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
	ElektraKey * our_root = elektraKeyNew ("user:/our", ELEKTRA_KEY_END);
	ElektraKey * their_root = elektraKeyNew ("user:/their", ELEKTRA_KEY_END);
	ElektraKey * base_root = elektraKeyNew ("user:/base", ELEKTRA_KEY_END);
	ElektraKey * result_root = elektraKeyNew ("user:/result", ELEKTRA_KEY_END);
	ElektraKey * informationKey = elektraKeyNew ("/", ELEKTRA_KEY_END);
	ElektraKeyset * our = elektraKeysetNew (1, elektraKeyNew ("user:/our/key", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_META, "order", our_order, ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * their = elektraKeysetNew (1, elektraKeyNew ("user:/their/key", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_META, "order", their_order, ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * base = elektraKeysetNew (1, elektraKeyNew ("user:/base/key", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_META, "order", base_order, ELEKTRA_KEY_END), ELEKTRA_KS_END);

	ElektraKeyset * result = elektraMerge (our, our_root, their, their_root, base, base_root, result_root, strategy, informationKey);

	if (expected_result == NULL)
	{
		yield_error ("expected_result parameter must not be null");
	}
	else
	{
		const ElektraKey * resultKey = elektraKeysetLookupByName (result, "user:/result/key", 0);
		if (resultKey == NULL)
		{
			yield_error ("Lookup must succeed");
		}
		const ElektraKey * metaKey = elektraKeyGetMeta (resultKey, "order");
		if (metaKey == NULL)
		{
			yield_error ("Meta key must exist");
		}
		char * resultValue = elektraMalloc (default_result_size);
		elektraKeyGetString (metaKey, resultValue, default_result_size);
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

	elektraKeysetDel (our);
	elektraKeysetDel (their);
	elektraKeysetDel (base);
	elektraKeysetDel (result);
	elektraKeyDel (our_root);
	elektraKeyDel (their_root);
	elektraKeyDel (base_root);
	elektraKeyDel (result_root);
	elektraKeyDel (informationKey);
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
	ElektraKey * our_root = elektraKeyNew ("user:/our", ELEKTRA_KEY_END);
	ElektraKey * their_root = elektraKeyNew ("user:/their", ELEKTRA_KEY_END);
	ElektraKey * base_root = elektraKeyNew ("user:/base", ELEKTRA_KEY_END);
	ElektraKey * result_root = elektraKeyNew ("user:/result", ELEKTRA_KEY_END);
	ElektraKey * informationKey = elektraKeyNew (0, ELEKTRA_KEY_END);
	ElektraKeyset * our = elektraKeysetNew (5, elektraKeyNew ("user:/our/#0", ELEKTRA_KEY_VALUE, "a", ELEKTRA_KEY_END), elektraKeyNew ("user:/our/#1", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END),
			      elektraKeyNew ("user:/our/#2", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END), elektraKeyNew ("user:/our/#3", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_END),
			      elektraKeyNew ("user:/our/#4", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * their =
		elektraKeysetNew (4, elektraKeyNew ("user:/their/#0", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END), elektraKeyNew ("user:/their/#1", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/their/#2", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_END), elektraKeyNew ("user:/their/#3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * base =
		elektraKeysetNew (4, elektraKeyNew ("user:/base/#0", ELEKTRA_KEY_VALUE, "0", ELEKTRA_KEY_END), elektraKeyNew ("user:/base/#1", ELEKTRA_KEY_VALUE, "1", ELEKTRA_KEY_END),
		       elektraKeyNew ("user:/base/#2", ELEKTRA_KEY_VALUE, "2", ELEKTRA_KEY_END), elektraKeyNew ("user:/base/#3", ELEKTRA_KEY_VALUE, "3", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	ElektraKeyset * result =
		elektraMerge (our, our_root, their, their_root, base, base_root, result_root, MERGE_STRATEGY_ABORT, informationKey);

	elektraKeysetDel (our);
	elektraKeysetDel (their);
	elektraKeysetDel (base);
	elektraKeysetDel (result);
	elektraKeyDel (our_root);
	elektraKeyDel (their_root);
	elektraKeyDel (base_root);
	elektraKeyDel (result_root);
	elektraKeyDel (informationKey);
}

static void testValuesWithGivenLength (int size)
{
	printf ("Executing %s with size %d\n", __func__, size);
	ElektraKey * our_root = elektraKeyNew ("user:/tests/our", ELEKTRA_KEY_END);
	ElektraKey * their_root = elektraKeyNew ("user:/tests/their", ELEKTRA_KEY_END);
	ElektraKey * base_root = elektraKeyNew ("user:/tests/base", ELEKTRA_KEY_END);
	ElektraKey * result_root = elektraKeyNew ("user:/tests/result", ELEKTRA_KEY_END);
	ElektraKey * informationKey = elektraKeyNew (0, ELEKTRA_KEY_END);
	char * value = elektraCalloc (size);
	memset (value, 'a', size - 1); // leave the last element \0
	// clang-format off
	ElektraKeyset * our = elektraKeysetNew (3,
		elektraKeyNew ("user:/tests/our/#0", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/our/#1", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/our/#2", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	ElektraKeyset * their = elektraKeysetNew (3,
		elektraKeyNew ("user:/tests/their/#0", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/their/#1", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/their/#2", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	ElektraKeyset * base = elektraKeysetNew (3,
		elektraKeyNew ("user:/tests/base/#0", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/base/#1", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END),
		elektraKeyNew ("user:/tests/base/#2", ELEKTRA_KEY_VALUE, value, ELEKTRA_KEY_END),
		ELEKTRA_KS_END);
	// clang-format on
	elektraFree (value);
	ElektraKeyset * result =
		elektraMerge (our, our_root, their, their_root, base, base_root, result_root, MERGE_STRATEGY_ABORT, informationKey);

	elektraKeysetDel (our);
	elektraKeysetDel (their);
	elektraKeysetDel (base);
	elektraKeysetDel (result);
	elektraKeyDel (our_root);
	elektraKeyDel (their_root);
	elektraKeyDel (base_root);
	elektraKeyDel (result_root);
	elektraKeyDel (informationKey);
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

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
#define OUR_ROOT "user/our"
#define THEIR_ROOT "user/their"
#define BASE_ROOT "user/base"
#define RESULT_ROOT "user/result"
#define OUR_KEY1 "user/our/key1"
#define THEIR_KEY1 "user/their/key1"
#define BASE_KEY1 "user/base/key1"
#define RESULT_KEY1 "user/result/key1"
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

void printKs (KeySet * ks)
{
	Key * cur = 0;
	fprintf (stdout, "DEBUG: Iterate over all keys:\n");
	ksRewind (ks);
	while ((cur = ksNext (ks)) != 0)
	{ /* Iterates over all keys and prints their name */
		fprintf (stdout, "DEBUG:   %s\n", keyName (cur));
	}
}


/**
 * When there is a single key in each key set and all key names are equal
 *
 * The parameters are the values for the single key in each set
 *
 * If a parameter is null then no key is set (!= key with empty value)
 *
 * If expected_result = NULL then the merge result must be NULL
 */
static void simple_test (char * our_value, char * their_value, char * base_value, int strategy, char * expected_result)
{
	printf ("Executing %s with our=%s their=%s base=%s\n", __func__, our_value, their_value, base_value);
	Key * our_root = keyNew ("user/our", KEY_END);
	Key * their_root = keyNew ("user/their", KEY_END);
	Key * base_root = keyNew ("user/base", KEY_END);
	Key * result_root = keyNew ("user/result", KEY_END);
	KeySet * our = ksNew (0, KS_END);
	KeySet * their = ksNew (0, KS_END);
	KeySet * base = ksNew (0, KS_END);
	if (our_value != NULL)
	{
		ksAppendKey (our, keyNew ("user/our/key", KEY_VALUE, our_value, KEY_END));
	}
	if (their_value != NULL)
	{
		ksAppendKey (their, keyNew ("user/their/key", KEY_VALUE, their_value, KEY_END));
	}
	if (base_value != NULL)
	{
		ksAppendKey (base, keyNew ("user/base/key", KEY_VALUE, base_value, KEY_END));
	}
	KeySet * result = kdbMerge (our, our_root, their, their_root, base, base_root, result_root, strategy);

	if (expected_result == NULL)
	{
		char msg[200];
		snprintf (msg, 200, "Executing %s with our=%s their=%s base=%s and strategy %i. Expected the merge result to be NULL but it was existant.",
			  __func__, our_value, their_value, base_value, strategy);
		succeed_if (result == NULL, msg);
		printKs(result);
	}
	else
	{
		Key * resultKey = ksLookupByName (result, "user/result/key", 0);
		if (resultKey == NULL)
		{
			yield_error ("Looked up key must not be NULL");
		}
		else
		{
			char * resultValue = elektraMalloc (default_result_size);
			keyGetString (resultKey, resultValue, default_result_size);
			char msg[200];
			snprintf (msg, 200, "Executing %s with our=%s their=%s base=%s and strategy %i. Expected result was %s but in reality it was %s.\n",
				  __func__, our_value, their_value, base_value, strategy, expected_result, resultValue);
			succeed_if (strcmp (resultValue, expected_result) == 0, msg);
			elektraFree (resultValue);
		}
		keyDel (resultKey); // Necessary?
	}

	ksDel (our);
	ksDel (their);
	ksDel (base);
	ksDel (result);
	keyDel (our_root);
	keyDel (their_root);
	keyDel (base_root);
	keyDel (result_root);
}

/**
 * Changing a comment in a own configuration file leaves this comment be if an upgrade happens and
 * the comment changes
 * Similar to ucf https://packages.debian.org/sid/ucf
 * Base                 Ours                  Theirs                Result
 * key1=1               key1=1                key1=1                key1=1
 * comment=some_comment comment=other_comment comment=some_comment comment=other_comment
 *
 */
 static void test_15 (void)
{
	printf ("In test function %s\n", __func__);
	Key * a = keyNew (OUR_ROOT, KEY_END);
	Key * b = keyNew (THEIR_ROOT, KEY_END);
	Key * c = keyNew (BASE_ROOT, KEY_END);
	Key * d = keyNew (RESULT_ROOT, KEY_END);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, OTHER_COMMENT, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, a, their, b, base, c, d, MERGE_STRATEGY_IRRELEVANT);

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	if (resultKey == NULL)
	{
		yield_error ("Should not be NULL");
	}
	else
	{
		char * resultValue = elektraMalloc (default_result_size);
		const Key * metakey = keyGetMeta (resultKey, COMMENT);
		if (metakey == 0) yield_error ("Meta key must not be null");
		succeed_if_same_string (keyValue (metakey), OTHER_COMMENT);
		elektraFree (resultValue);
	}

	ksDel (our);
	ksDel (their);
	ksDel (base);
	ksDel (result);
	keyDel (a);
	keyDel (b);
	keyDel (c);
	keyDel (d);
}

///**
// * When local changes have been done and the comment gets updated then really do this update
// * Base                 Ours                 Theirs                Result
// * key1=1               key1=1               key1=1                key1=1
// * comment=some_comment comment=some_comment comment=other_comment comment=other_comment
// *
// */
// static void test_16 (void)
//{
//	printf ("In test function %s\n", __func__);
//	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
//	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, OTHER_COMMENT, KEY_END), KS_END);
//	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
//	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
//				    keyNew (RESULT_ROOT, KEY_END), MERGE_STRATEGY_IRRELEVANT);
//
//	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
//	if (resultKey == NULL)
//	{
//		yield_error ("Should not be NULL");
//	}
//	else
//	{
//		char * resultValue = elektraMalloc (default_result_size);
//		const Key * metakey = keyGetMeta (resultKey, COMMENT);
//		if (metakey == 0) yield_error ("Meta key must not be null");
//		succeed_if_same_string (keyValue (metakey), OTHER_COMMENT);
//		elektraFree (resultValue);
//	}
//
//	ksDel (our);
//	ksDel (their);
//	ksDel (base);
//}
//
///**
// * When local changes have been done and the comment gets updated then really do this update
// * Base                 Ours                 Theirs                Result
// * key1=1               key1=1               key1=1                key1=1
// * comment=some_comment comment=other_comment comment=some_comment comment=other_comment
// *
// */
// static void test_16a (void)
//{
//	printf ("In test function %s\n", __func__);
//	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, OTHER_COMMENT, KEY_END), KS_END);
//	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
//	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
//	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
//				    keyNew (RESULT_ROOT, KEY_END), MERGE_STRATEGY_IRRELEVANT);
//
//	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
//	if (resultKey == NULL)
//	{
//		yield_error ("Should not be NULL");
//	}
//	else
//	{
//		char * resultValue = elektraMalloc (default_result_size);
//		const Key * metakey = keyGetMeta (resultKey, COMMENT);
//		if (metakey == 0) yield_error ("Meta key must not be null");
//		succeed_if_same_string (keyValue (metakey), OTHER_COMMENT);
//		elektraFree (resultValue);
//	}
//
//	ksDel (our);
//	ksDel (their);
//	ksDel (base);
//}
//
///**
// * Base                 Ours                 Theirs                Result
// * key1=1               key1=1               key1=1                key1=1
// * comment=other_comment comment=some_comment comment=some_comment comment=other_comment
// *
// */
// static void test_16b (void)
//{
//	printf ("In test function %s\n", __func__);
//	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
//	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
//	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, OTHER_COMMENT, KEY_END), KS_END);
//	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
//				    keyNew (RESULT_ROOT, KEY_END), MERGE_STRATEGY_IRRELEVANT);
//
//	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
//	if (resultKey == NULL)
//	{
//		yield_error ("Should not be NULL");
//	}
//	else
//	{
//		char * resultValue = elektraMalloc (default_result_size);
//		const Key * metakey = keyGetMeta (resultKey, COMMENT);
//		if (metakey == 0) yield_error ("Meta key must not be null");
//		succeed_if_same_string (keyValue (metakey), OTHER_COMMENT);
//		elektraFree (resultValue);
//	}
//
//	ksDel (our);
//	ksDel (their);
//	ksDel (base);
//}
//
///**
// * Base                 Ours                 Theirs                Result
// * key1=1               key1=1               key1=1                key1=1
// * comment=other_comment comment=some_comment comment=some_comment comment=other_comment
// *
// */
// static void test_16c (void)
//{
//	printf ("In test function %s\n", __func__);
//	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
//	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
//	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, OTHER_COMMENT, KEY_END), KS_END);
//	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
//				    keyNew (RESULT_ROOT, KEY_END), MERGE_STRATEGY_IRRELEVANT);
//
//	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
//	if (resultKey == NULL)
//	{
//		yield_error ("Should not be NULL");
//	}
//	else
//	{
//		char * resultValue = elektraMalloc (default_result_size);
//		const Key * metakey = keyGetMeta (resultKey, COMMENT);
//		if (metakey == 0) yield_error ("Meta key must not be null");
//		succeed_if_same_string (keyValue (metakey), OTHER_COMMENT);
//		elektraFree (resultValue);
//	}
//
//	ksDel (our);
//	ksDel (their);
//	ksDel (base);
//}
//
///**
// * This is the example from the file build/doc/html/doc_tutorials_merge_md.html
// *
// * Key 4 is deleted in our and changed in their key set. Strategy abort.
// */
// static void test_17 (void)
//{
//	printf ("In test function %s\n", __func__);
//	KeySet * their = ksNew (
//		5, keyNew ("user/their/key1", KEY_VALUE, "1", KEY_END), keyNew ("user/their/key2", KEY_VALUE, "pie", KEY_END),
//		keyNew ("user/their/key4", KEY_VALUE, "banana", KEY_END), keyNew ("user/their/key5", KEY_VALUE, "5", KEY_END), KS_END);
//
//	KeySet * our =
//		ksNew (4, keyNew ("user/our/key1", KEY_VALUE, "apple", KEY_END), keyNew ("user/our/key2", KEY_VALUE, "2", KEY_END),
//		       keyNew ("user/our/key3", KEY_VALUE, "3", KEY_END), keyNew ("user/our/key5", KEY_VALUE, "fish", KEY_END), KS_END);
//
//	KeySet * base = ksNew (5, keyNew ("user/base/key1", KEY_VALUE, "1", KEY_END), keyNew ("user/base/key2", KEY_VALUE, "2", KEY_END),
//			       keyNew ("user/base/key3", KEY_VALUE, "3", KEY_END), keyNew ("user/base/key4", KEY_VALUE, "4", KEY_END),
//			       keyNew ("user/base/key5", KEY_VALUE, "5", KEY_END), KS_END);
//	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
//				    keyNew (RESULT_ROOT, KEY_END), MERGE_STRATEGY_ABORT);
//
//	succeed_if (result == NULL, "There should be a conflict and that should lead to NULL as keyset!");
//
//	ksDel (our);
//	ksDel (their);
//	ksDel (base);
//}
//
///**
// * This is the example from the file build/doc/html/doc_tutorials_merge_md.html
// *
// * Key 4 is deleted in our and changed in their key set. Strategy our.
// */
// static void test_17a (void)
//{
//	printf ("In test function %s\n", __func__);
//	KeySet * their = ksNew (
//		5, keyNew ("user/their/key1", KEY_VALUE, "1", KEY_END), keyNew ("user/their/key2", KEY_VALUE, "pie", KEY_END),
//		keyNew ("user/their/key4", KEY_VALUE, "banana", KEY_END), keyNew ("user/their/key5", KEY_VALUE, "5", KEY_END), KS_END);
//
//	KeySet * our =
//		ksNew (4, keyNew ("user/our/key1", KEY_VALUE, "apple", KEY_END), keyNew ("user/our/key2", KEY_VALUE, "2", KEY_END),
//		       keyNew ("user/our/key3", KEY_VALUE, "3", KEY_END), keyNew ("user/our/key5", KEY_VALUE, "fish", KEY_END), KS_END);
//
//	KeySet * base = ksNew (5, keyNew ("user/base/key1", KEY_VALUE, "1", KEY_END), keyNew ("user/base/key2", KEY_VALUE, "2", KEY_END),
//			       keyNew ("user/base/key3", KEY_VALUE, "3", KEY_END), keyNew ("user/base/key4", KEY_VALUE, "4", KEY_END),
//			       keyNew ("user/base/key5", KEY_VALUE, "5", KEY_END), KS_END);
//	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
//				    keyNew (RESULT_ROOT, KEY_END), MERGE_STRATEGY_OUR);
//
//	if (result == NULL)
//	{
//		yield_error ("Not the whole key set should be null. Only key 4 must not be found!\n");
//	}
//	Key * resultKey = ksLookupByName (result, "user/result/key4", 0);
//	succeed_if (resultKey == NULL, "key 4 must not be found!");
//
//	ksDel (our);
//	ksDel (their);
//	ksDel (base);
//}
///**
// * This is the example from the file build/doc/html/doc_tutorials_merge_md.html
// * At the moment there is only the preserve strategy
// *
// * Key 4 is deleted in our and changed in their key set. Strategy their.
// */
// static void test_17b (void)
//{
//	printf ("In test function %s\n", __func__);
//	KeySet * their = ksNew (
//		5, keyNew ("user/their/key1", KEY_VALUE, "1", KEY_END), keyNew ("user/their/key2", KEY_VALUE, "pie", KEY_END),
//		keyNew ("user/their/key4", KEY_VALUE, "banana", KEY_END), keyNew ("user/their/key5", KEY_VALUE, "5", KEY_END), KS_END);
//
//	KeySet * our =
//		ksNew (4, keyNew ("user/our/key1", KEY_VALUE, "apple", KEY_END), keyNew ("user/our/key2", KEY_VALUE, "2", KEY_END),
//		       keyNew ("user/our/key3", KEY_VALUE, "3", KEY_END), keyNew ("user/our/key5", KEY_VALUE, "fish", KEY_END), KS_END);
//
//	KeySet * base = ksNew (5, keyNew ("user/base/key1", KEY_VALUE, "1", KEY_END), keyNew ("user/base/key2", KEY_VALUE, "2", KEY_END),
//			       keyNew ("user/base/key3", KEY_VALUE, "3", KEY_END), keyNew ("user/base/key4", KEY_VALUE, "4", KEY_END),
//			       keyNew ("user/base/key5", KEY_VALUE, "5", KEY_END), KS_END);
//	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
//				    keyNew (RESULT_ROOT, KEY_END), MERGE_STRATEGY_THEIR);
//
//	Key * resultKey = ksLookupByName (result, "user/result/key4", 0);
//	if (resultKey == NULL)
//	{
//		yield_error ("Should not be NULL");
//	}
//	else
//	{
//		char * resultValue = elektraMalloc (default_result_size);
//		keyGetString (resultKey, resultValue, default_result_size);
//		succeed_if_same_string ("banana", resultValue);
//		elektraFree (resultValue);
//	}
//
//	ksDel (our);
//	ksDel (their);
//	ksDel (base);
//}
///**
// * Adding two values to an array independently
// * base              our              their            result
// * [First Element]   [First Element]  [First Element]  [First Element]
// * [Second Element]  [Second Element] [Second Element] [Second Element]
// *                   [Third Element]                   [Third Element]
// *                                    [Fourth Element] [Fourth Element]
// */
// static void test_18 (void)
//{
//	printf ("In test function %s\n", __func__);
//	KeySet * base = ksNew (2, keyNew ("user/base/key1/#1", KEY_VALUE, "First Element", KEY_END),
//			       keyNew ("user/base/key1/#2", KEY_VALUE, "Second Element", KEY_END), KS_END);
//
//	KeySet * our = ksNew (3, keyNew ("user/our/key1/#1", KEY_VALUE, "First Element", KEY_END),
//			      keyNew ("user/our/key1/#2", KEY_VALUE, "Second Element", KEY_END),
//			      keyNew ("user/our/key1/#3", KEY_VALUE, "Third Element", KEY_END), KS_END);
//
//	KeySet * their = ksNew (3, keyNew ("user/their/key1/#1", KEY_VALUE, "First Element", KEY_END),
//				keyNew ("user/their/key1/#2", KEY_VALUE, "Second Element", KEY_END),
//				keyNew ("user/their/key1/#4", KEY_VALUE, "Fourth Element", KEY_END), KS_END);
//	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
//				    keyNew (RESULT_ROOT, KEY_END), MERGE_STRATEGY_IRRELEVANT);
//
//	Key * resultKey = ksLookupByName (result, "user/result/key1/#3", 0);
//	char * resultValue = elektraMalloc (default_result_size);
//	if (keyGetString (resultKey, resultValue, default_result_size) <= 0)
//	{
//		yield_error ("Getting value has not worked")
//	}
//	else
//	{
//		succeed_if_same_string (resultValue, "Third Element");
//	}
//	elektraFree (resultValue);
//
//	Key * resultKey2 = ksLookupByName (result, "user/result/key1/#4", 0);
//	char * resultValue2 = elektraMalloc (default_result_size);
//	if (keyGetString (resultKey2, resultValue2, default_result_size) <= 1)
//	{
//		yield_error ("Getting second value has not worked")
//	}
//	else
//	{
//		succeed_if_same_string (resultValue2, "Fourth Element");
//	}
//	elektraFree (resultValue2);
//
//	ksDel (our);
//	ksDel (their);
//	ksDel (base);
//}

int main (int argc, char ** argv)
{
	printf ("CMERGE       TESTS\n");
	printf ("==================\n\n");

	/** Always check if all tests are listed here */
	init (argc, argv);
	simple_test ("1", "1", "1", MERGE_STRATEGY_IRRELEVANT, "1");
	simple_test ("1", "2", "1", MERGE_STRATEGY_IRRELEVANT, "2");
	simple_test ("2", "1", "1", MERGE_STRATEGY_IRRELEVANT, "2");
	// Merge strategy is abort. There should be a conflict and that should lead to NULL as keyset
	simple_test ("1", "2", "3", MERGE_STRATEGY_ABORT, NULL);
	simple_test ("1", "2", "3", MERGE_STRATEGY_OUR, "1");
	// simple_test ("1", "2", "3", MERGE_STRATEGY_THEIR, "2");
	// simple_test ("1", NULL, "1", MERGE_STRATEGY_IRRELEVANT, NULL);
	// simple_test (NULL, "1", "1", MERGE_STRATEGY_IRRELEVANT, NULL);
	// simple_test (NULL, "1", NULL, MERGE_STRATEGY_IRRELEVANT, "1");
	// simple_test ("1", NULL, NULL, MERGE_STRATEGY_IRRELEVANT, "1");
	// /**
	//  *   Begin section of conflicts that are not overlaps
	//  *   According to https://www.gnu.org/software/diffutils/manual/html_node/diff3-Merging.html
	//  *   this is a conflict but not an overlap
	//  */
	// /**
	//  * End section of conflicts that are not overlaps
	//  */
	// simple_test ("1", "1", "2", MERGE_STRATEGY_ABORT, NULL);
	// simple_test ("1", "1", "2", MERGE_STRATEGY_OUR, "1");
	// simple_test ("1", "1", "2", MERGE_STRATEGY_THEIR, "1");
	// simple_test (NULL, NULL, "1", MERGE_STRATEGY_OUR, NULL);
	// simple_test (NULL, NULL, "1", MERGE_STRATEGY_ABORT, NULL);
	// simple_test ("1", "1", NULL, MERGE_STRATEGY_ABORT, NULL);
	// simple_test ("1", "1", NULL, MERGE_STRATEGY_BASE, NULL);
	// simple_test ("1", "1", NULL, MERGE_STRATEGY_OUR, "1");
	// simple_test ("1", "1", NULL, MERGE_STRATEGY_THEIR, "1");
	// simple_test (NULL, NULL, "1", MERGE_STRATEGY_BASE, "1");
	// // Overlap conflict
	// simple_test ("1", "2", NULL, MERGE_STRATEGY_ABORT, "1"); // TODO Result is NULL
	// simple_test ("1", "2", NULL, MERGE_STRATEGY_OUR, "1");
	// simple_test ("1", "2", NULL, MERGE_STRATEGY_THEIR, "2");

	// test_15 ();
	//	test_16 ();
	//	test_17 ();
	//	test_17a ();
	//	test_17b ();
	//	test_18 ();


	printf ("\ntest_merge RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

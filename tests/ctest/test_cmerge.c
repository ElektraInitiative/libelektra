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
#define SOME_STRING "abc"
#define SOME_STRING_SPACE_BEFORE " abc"
#define COMMENT "comment"
#define SOME_COMMENT "some_comment"
#define OTHER_COMMENT "other_comment"
#define OTHER_COMMENT_LENGTH 13

void printKs (KeySet * ks)
{
	Key * cur = 0;
	fprintf (stdout, "DEBUG: Iterate over all keys:\n");
	ksRewind (ks);
	while ((cur = ksNext (ks)) != 0)
	{ /* Iterates over all keys and prints their name */
		fprintf (stdout, "DEBUG: --%s\n", keyName (cur));
	}
}

/**
 *   Base     Ours     Theirs     Result
 *   key1=1   key1=1   key1=1     key1=1
 */
static void test_1 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	keyGetString (resultKey, resultValue, default_result_size);
	succeed_if_same_string (resultValue, ORIGINAL_VALUE);

	elektraFree (resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *   key1=1   key1=1   key1=2     key1=2
 */
static void test_2 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, CHANGED_VALUE, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	// TODO: Add this null check to other tests
	if (resultKey == NULL)
	{
		yield_error ("Should not be NULL");
	}
	else
	{
		char * resultValue = elektraMalloc (default_result_size);
		keyGetString (resultKey, resultValue, default_result_size);
		succeed_if_same_string (CHANGED_VALUE, resultValue);
		elektraFree (resultValue);
	}

	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *   key1=1   key1=2   key1=1     key1=2
 */
static void test_3 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, CHANGED_VALUE, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	keyGetString (resultKey, resultValue, default_result_size);
	succeed_if_same_string (resultValue, CHANGED_VALUE);

	elektraFree (resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *   key1=2   key1=1   key1=1     key1=1
 */
static void test_4 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, CHANGED_VALUE, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, CHANGED_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	keyGetString (resultKey, resultValue, default_result_size);
	succeed_if_same_string (ORIGINAL_VALUE, resultValue);

	elektraFree (resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *   key1=1   key1=2   key1=3     Conflict
 */
static void test_5 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, CHANGED_VALUE, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, MORE_CHANGED_VALUE, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	succeed_if (result == NULL, "There should be a conflict and that should lead to NULL as keyset!");

	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *   key1=1   key1=1
 */
static void test_6 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * their = ksNew (0, KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	succeed_if (ksLookupByName (result, RESULT_KEY1, 0) == 0, "Key should not be in result!");

	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *   key1=1            key1=1
 */
static void test_7 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * our = ksNew (0, KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	succeed_if (ksLookupByName (result, RESULT_KEY1, 0) == 0, "Key should not be in result!");

	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *   key1=1
 */
static void test_8 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * their = ksNew (0, KS_END);
	KeySet * our = ksNew (0, KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	succeed_if (ksLookupByName (result, RESULT_KEY1, 0) == 0, "Key should not be in result!");

	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *                     key1=1     key1=1
 */
static void test_9 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * base = ksNew (0, KS_END);
	KeySet * our = ksNew (0, KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	keyGetString (resultKey, resultValue, default_result_size);
	succeed_if_same_string (resultValue, ORIGINAL_VALUE);

	elektraFree (resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *            key1=1              key1=1
 */
static void test_10 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * base = ksNew (0, KS_END);
	KeySet * their = ksNew (0, KS_END);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	keyGetString (resultKey, resultValue, default_result_size);
	succeed_if_same_string (ORIGINAL_VALUE, resultValue);

	elektraFree (resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *            key1=1   key1=1     key1=1
 */
static void test_11 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * base = ksNew (0, KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	keyGetString (resultKey, resultValue, default_result_size);
	succeed_if_same_string (ORIGINAL_VALUE, resultValue);

	elektraFree (resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 *   Base     Ours     Theirs     Result
 *            key1=1   key1=2     Conflict
 */
static void test_12 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * base = ksNew (0, KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, CHANGED_VALUE, KEY_END), KS_END);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	succeed_if (result == NULL, "There should be a conflict and that should lead to NULL as keyset!");

	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 * Spaces make no semantic difference.
 * Keep our version if the new version is semantically equal.
 * Base        Ours        Theirs       Result
 * key1="abc"  key1="abc"  key1=" abc"  key1="abc"
 */
static void test_13 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, SOME_STRING, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, SOME_STRING_SPACE_BEFORE, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, SOME_STRING, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	keyGetString (resultKey, resultValue, default_result_size);
	succeed_if_same_string (SOME_STRING, resultValue);

	elektraFree (resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 * Spaces make no semantic difference.
 * Keep our version if the new version is semantically equal.
 * Base        Ours        Theirs       Result
 * key1="abc"  key1=" abc" key1="abc"   key1=" abc"
 */
static void test_14 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, SOME_STRING_SPACE_BEFORE, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, SOME_STRING, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, SOME_STRING, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	keyGetString (resultKey, resultValue, default_result_size);
	succeed_if_same_string (SOME_STRING_SPACE_BEFORE, resultValue);

	elektraFree (resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
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
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, OTHER_COMMENT, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	const Key * metakey = keyGetMeta (resultKey, COMMENT);
	if (metakey == 0) yield_error ("Meta key must not be null");
	succeed_if_same_string (keyValue (metakey), OTHER_COMMENT);

	elektraFree (resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 * When local changes have been done and the comment gets updated then really do this update
 * Base                 Ours                 Theirs                Result
 * key1=1               key1=1               key1=1                key1=1
 * comment=some_comment comment=some_comment comment=other_comment comment=other_comment
 *
 */
static void test_16 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * our = ksNew (1, keyNew (OUR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (THEIR_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, OTHER_COMMENT, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (BASE_KEY1, KEY_VALUE, ORIGINAL_VALUE, KEY_META, COMMENT, SOME_COMMENT, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, RESULT_KEY1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	const Key * metakey = keyGetMeta (resultKey, COMMENT);
	if (metakey == 0) yield_error ("Meta key must not be null");
	succeed_if_same_string (keyValue (metakey), OTHER_COMMENT);

	elektraFree (resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 * This is the example from the file build/doc/html/doc_tutorials_merge_md.html
 * At the moment there is only the preserve strategy
 *
 * Key 4 is deleted in our and changed in their key set => Conflict
 */
static void test_17 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * their = ksNew (
		5, keyNew ("user/their/key1", KEY_VALUE, "1", KEY_END), keyNew ("user/their/key2", KEY_VALUE, "pie", KEY_END),
		keyNew ("user/their/key4", KEY_VALUE, "banana", KEY_END), keyNew ("user/their/key5", KEY_VALUE, "5", KEY_END), KS_END);

	KeySet * our =
		ksNew (4, keyNew ("user/our/key1", KEY_VALUE, "apple", KEY_END), keyNew ("user/our/key2", KEY_VALUE, "2", KEY_END),
		       keyNew ("user/our/key3", KEY_VALUE, "3", KEY_END), keyNew ("user/our/key5", KEY_VALUE, "fish", KEY_END), KS_END);

	KeySet * base = ksNew (5, keyNew ("user/base/key1", KEY_VALUE, "1", KEY_END), keyNew ("user/base/key2", KEY_VALUE, "2", KEY_END),
			       keyNew ("user/base/key3", KEY_VALUE, "3", KEY_END), keyNew ("user/base/key4", KEY_VALUE, "4", KEY_END),
			       keyNew ("user/base/key5", KEY_VALUE, "5", KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	printKs (result);
	succeed_if (result == NULL, "There should be a conflict and that should lead to NULL as keyset!");

	ksDel (our);
	ksDel (their);
	ksDel (base);
}

/**
 * Adding two values to an array independently
 * base              our              their            result
 * [First Element]   [First Element]  [First Element]  [First Element]
 * [Second Element]  [Second Element] [Second Element] [Second Element]
 *                   [Third Element]                   [Third Element]
 *                                    [Fourth Element] [Fourth Element]
 */
static void test_18 (void)
{
	printf ("In test function %s\n", __func__);
	KeySet * base = ksNew (2, keyNew ("user/base/key1/#1", KEY_VALUE, "First Element", KEY_END),
			       keyNew ("user/base/key1/#2", KEY_VALUE, "Second Element", KEY_END), KS_END);

	KeySet * our = ksNew (3, keyNew ("user/our/key1/#1", KEY_VALUE, "First Element", KEY_END),
			      keyNew ("user/our/key1/#2", KEY_VALUE, "Second Element", KEY_END),
			      keyNew ("user/our/key1/#3", KEY_VALUE, "Third Element", KEY_END), KS_END);

	KeySet * their = ksNew (3, keyNew ("user/their/key1/#1", KEY_VALUE, "First Element", KEY_END),
				keyNew ("user/their/key1/#2", KEY_VALUE, "Second Element", KEY_END),
				keyNew ("user/their/key1/#4", KEY_VALUE, "Fourth Element", KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (OUR_ROOT, KEY_END), their, keyNew (THEIR_ROOT, KEY_END), base, keyNew (BASE_ROOT, KEY_END),
				    keyNew (RESULT_ROOT, KEY_END));

	Key * resultKey = ksLookupByName (result, "user/result/key1/#3", 0);
	char * resultValue = elektraMalloc (default_result_size);
	if (keyGetString (resultKey, resultValue, default_result_size) <= 0)
	{
		yield_error ("Getting value has not worked")
	}
	else
	{
		succeed_if_same_string (resultValue, "Third Element");
	}
	elektraFree (resultValue);

	Key * resultKey2 = ksLookupByName (result, "user/result/key1/#4", 0);
	char * resultValue2 = elektraMalloc (default_result_size);
	if (keyGetString (resultKey2, resultValue2, default_result_size) <= 1)
	{
		yield_error ("Getting second value has not worked")
	}
	else
	{
		succeed_if_same_string (resultValue2, "Fourth Element");
	}
	elektraFree (resultValue2);

	ksDel (our);
	ksDel (their);
	ksDel (base);
}

int main (int argc, char ** argv)
{
	printf ("CMERGE       TESTS\n");
	printf ("==================\n\n");

	/** Always check if all tests are listed here */
	init (argc, argv);
	test_1 ();
	test_2 ();
	test_3 ();
	test_4 ();
	test_5 ();
	test_6 ();
	test_7 ();
	test_8 ();
	test_9 ();
	test_10 ();
	test_11 ();
	test_12 ();
	test_13 ();
	test_14 ();
	// test_15 (); // not working yet
	// test_16 (); // not working yet
	test_17 (); // TODO Continue here 
	test_18 ();


	printf ("\ntest_merge RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

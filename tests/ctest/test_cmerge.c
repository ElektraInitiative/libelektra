/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbmerge.h>
#include <tests.h>

#define default_result_size 5 // Enough space for possible strange results
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

void printKs (KeySet * ks)
{
	Key * cur = 0;
	printf ("Iterate over all keys:\n");
	ksRewind (ks);
	while ((cur = ksNext (ks)) != 0)
	{ /* Iterates over all keys and prints their name */
		printf ("--%s\n", keyName (cur));
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

	printf("results size is %ld\n", ksGetSize(result));
	printKs(result);
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

int main (int argc, char ** argv)
{
	printf ("CMERGE        TESTS\n");
	printf ("==================\n\n");

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

	printf ("\ntest_merge RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

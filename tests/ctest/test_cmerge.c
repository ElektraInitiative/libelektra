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
#define our_root "user/our"
#define their_root "user/their"
#define base_root "user/base"
#define result_root "user/result"
#define our_key1 "user/our/key1"
#define their_key1 "user/their/key1"
#define base_key1 "user/base/key1"
#define result_key1 "user/result/key1"
#define original_value "1"
#define changed_value "2"
#define more_changed_value "3"

/**
 *   Base     Ours     Theirs     Result
 *   key1=1   key1=1   key1=1     key1=1
 */
static void test_1 (void)
{
	KeySet * our = ksNew (1, keyNew (our_key1, KEY_VALUE, original_value, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (their_key1, KEY_VALUE, original_value, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (base_key1, KEY_VALUE, original_value, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (our_root, KEY_END), their, keyNew (their_root, KEY_END), base,
				    keyNew (base_root, KEY_END), keyNew (result_root, KEY_END));

	Key * resultKey = ksLookupByName (result, result_key1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	keyGetString (resultKey, resultValue, default_result_size);
	succeed_if_same_string (original_value, resultValue);

	elektraFree(resultValue);
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
	KeySet * our = ksNew (1, keyNew (our_key1, KEY_VALUE, original_value, KEY_END), KS_END);
	KeySet * their = ksNew (1, keyNew (their_key1, KEY_VALUE, changed_value, KEY_END), KS_END);
	KeySet * base = ksNew (1, keyNew (base_key1, KEY_VALUE, original_value, KEY_END), KS_END);
	KeySet * result = kdbMerge (our, keyNew (our_root, KEY_END), their, keyNew (their_root, KEY_END), base,
				    keyNew (base_root, KEY_END), keyNew (result_root, KEY_END));

	Key * resultKey = ksLookupByName (result, result_key1, 0);
	char * resultValue = elektraMalloc (default_result_size);
	keyGetString (resultKey, resultValue, default_result_size);
	succeed_if_same_string (changed_value, resultValue);

	elektraFree(resultValue);
	ksDel (our);
	ksDel (their);
	ksDel (base);
}
/**
 *   Base     Ours     Theirs     Result
 *   key1=1   key1=2   key1=1     key1=2
 */

/**
 *   Base     Ours     Theirs     Result
 *   key1=2   key1=1   key1=1     key1=1
 */

/**
 *   Base     Ours     Theirs     Result
 *   key1=1   key1=2   key1=3     Conflict
 */

/**
 *   Base     Ours     Theirs     Result
 *   key1=1   key1=1
 */

/**
 *   Base     Ours     Theirs     Result
 *   key1=1            key1=1
 */

/**
 *   Base     Ours     Theirs     Result
 *   key1=1
 */

/**
 *   Base     Ours     Theirs     Result
 *                     key1=1     key1=1
 */

/**
 *   Base     Ours     Theirs     Result
 *            key1=1              key1=1
 */

/**
 *   Base     Ours     Theirs     Result
 *            key1=1   key1=1     key1=1
 */

/**
 *   Base     Ours     Theirs     Result
 *            key1=1   key1=2     Conflict
 */

int main (int argc, char ** argv)
{
	printf ("CMERGE        TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);
	test_1 ();
	test_2 ();

	printf ("\ntest_merge RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

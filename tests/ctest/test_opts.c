/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdberrors.h>
#include <kdbopts.h>

#include "tests.h"

#define PROC_BASE_KEY "proc/tests/opts"
#define SPEC_BASE_KEY "spec/tests/opts"

#define NUMARGS(...) (sizeof ((void *[]){ __VA_ARGS__ }) / sizeof (void *))
#define ARGS(...) NUMARGS ("prog", __VA_ARGS__), ((const char *[]){ "prog", __VA_ARGS__, NULL })
#define NO_ARGS 1, ((const char *[]){ "prog" })

#define ENVP(...) ((const char *[]){ __VA_ARGS__, NULL })
#define NO_ENVP ((const char *[]){ NULL })

#define xstr(a) str (a)
#define str(a) #a

#define RUN_TEST(ks, args, envp)                                                                                                           \
	{                                                                                                                                  \
		Key * ek = keyNew (SPEC_BASE_KEY, KEY_END);                                                                                \
		if (elektraGetOpts (ks, args, envp, ek) != 0)                                                                              \
		{                                                                                                                          \
			yield_error ("error found");                                                                                       \
			output_error (ek);                                                                                                 \
		}                                                                                                                          \
		keyDel (ek);                                                                                                               \
	}

#define RUN_TEST_ERROR(ks, errorKey, args, envp)                                                                                           \
	{                                                                                                                                  \
		errorKey = keyNew (SPEC_BASE_KEY, KEY_END);                                                                                \
		if (elektraGetOpts (ks, args, envp, errorKey) == 0)                                                                        \
		{                                                                                                                          \
			yield_error ("should have failed");                                                                                \
		}                                                                                                                          \
	}

static inline Key * keyWithOpt (const char * name, const char shortOpt, const char * longOpt, const char * envVar)
{
	return keyNew (name, KEY_META, "opt", (const char[]){ shortOpt, '\0' }, KEY_META, "opt/long", longOpt, KEY_META, "env", envVar,
		       KEY_END);
}

static bool checkValue (KeySet * ks, const char * name, const char * expected)
{
	Key * key = ksLookupByName (ks, name, 0);
	if (key == NULL)
	{
		return false;
	}

	const char * actual = keyString (key);
	return actual != NULL && strcmp (actual, expected) == 0;
}

static bool checkError (Key * errorKey, const char * expectedNumber, const char * expectedReason)
{
	const Key * metaError = keyGetMeta (errorKey, "error");
	if (metaError == NULL)
	{
		return false;
	}

	const char * actualNumber = keyString (keyGetMeta (errorKey, "error/number"));
	const char * actualReason = keyString (keyGetMeta (errorKey, "error/reason"));

	bool result = strcmp (actualNumber, expectedNumber) == 0 && strcmp (actualReason, expectedReason) == 0;

	keyDel (errorKey);

	return result;
}

static void clearValues (KeySet * ks)
{
	cursor_t cursor = ksGetCursor (ks);

	ksRewind (ks);
	Key * cur;
	while ((cur = ksNext (ks)) != NULL)
	{
		keySetString (cur, NULL);
	}

	ksSetCursor (ks, cursor);
}

static void test_simple (void)
{
	KeySet * ks = ksNew (50, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", "APPLE"), KS_END);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option (combined) failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple=long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option (combined) failed");
	clearValues (ks);

	RUN_TEST (ks, NO_ARGS, ENVP ("APPLE=env"));
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "env"), "env-var failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_flag (void)
{
	Key * k = keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL);
	keySetMeta (k, "opt/arg", "none");
	KeySet * ks = ksNew (50, k, KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag (with arg) failed");
	clearValues (ks);

	Key * errorKey;
	RUN_TEST_ERROR (ks, errorKey, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkError (errorKey, xstr (ELEKTRA_ERROR_OPTS_UNKNOWN_OPTION), "Unknown short option: -s"),
		    "short flag (with arg, combined) should have failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "long flag failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "long flag (with arg) failed");
	clearValues (ks);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple=long"), NO_ENVP);
	succeed_if (checkError (errorKey, xstr (ELEKTRA_ERROR_OPTS_ILLEGAL_USE), "This option cannot have an argument: --apple"),
		    "long flag (with arg, combined) should have failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_flag_value (void)
{
	Key * k = keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL);
	keySetMeta (k, "opt/arg", "none");
	keySetMeta (k, "opt/flagvalue", "set");
	KeySet * ks = ksNew (50, k, KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value (with arg) failed");
	clearValues (ks);

	Key * errorKey;
	RUN_TEST_ERROR (ks, errorKey, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkError (errorKey, xstr (ELEKTRA_ERROR_OPTS_UNKNOWN_OPTION), "Unknown short option: -s"),
		    "short flag with value (with arg, combined) should have failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "long flag with value failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "long flag with value (with arg) failed");
	clearValues (ks);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple=long"), NO_ENVP);
	succeed_if (checkError (errorKey, xstr (ELEKTRA_ERROR_OPTS_ILLEGAL_USE), "This option cannot have an argument: --apple"),
		    "long flag with value (with arg, combined) should have failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_optional (void)
{
	Key * k = keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL);
	keySetMeta (k, "opt/arg", "optional");
	KeySet * ks = ksNew (50, k, KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag (with arg) failed");
	clearValues (ks);

	Key * errorKey;
	RUN_TEST_ERROR (ks, errorKey, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkError (errorKey, xstr (ELEKTRA_ERROR_OPTS_UNKNOWN_OPTION), "Unknown short option: -s"),
		    "short flag (with arg, combined) should have failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "long flag failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "long flag (with arg) failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple=long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option (combined) failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_optional_value (void)
{
	Key * k = keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL);
	keySetMeta (k, "opt/arg", "optional");
	keySetMeta (k, "opt/flagvalue", "set");
	KeySet * ks = ksNew (50, k, KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value (with arg) failed");
	clearValues (ks);

	Key * errorKey;
	RUN_TEST_ERROR (ks, errorKey, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkError (errorKey, xstr (ELEKTRA_ERROR_OPTS_UNKNOWN_OPTION), "Unknown short option: -s"),
		    "short flag with value (with arg, combined) should have failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "long flag with value failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "long flag with value (with arg) failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple=long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option (combined) failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_precedence (void)
{
	// TODO
}

static void test_repeated (void)
{
	// TODO
}

static void test_multiple (void)
{
	Key * k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "opt", "#1");
	keySetMeta (k, "opt/#0", "a");
	keySetMeta (k, "opt/#0/long", "apple");
	keySetMeta (k, "opt/#1", "b");
	keySetMeta (k, "opt/#1/long", "banana");
	keySetMeta (k, "env", "#1");
	keySetMeta (k, "env/#0", "APPLE");
	keySetMeta (k, "env/#1", "BANANA");
	KeySet * ks = ksNew (50, k, KS_END);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option (combined) failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple=long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option (combined) failed");
	clearValues (ks);

	RUN_TEST (ks, NO_ARGS, ENVP ("APPLE=env"));
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "env"), "env-var failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-b", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-bshort"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option (combined) failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--banana", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--banana=long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option (combined) failed");
	clearValues (ks);

	RUN_TEST (ks, NO_ARGS, ENVP ("BANANA=env"));
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "env"), "env-var failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_multiple_repeated (void)
{
	// TODO
}

static void test_illegal_spec (void)
{
	// TODO
}

static void test_illegal_use (void)
{
	// TODO
}

int main (int argc, char ** argv)
{
	printf (" OPTS   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_simple ();
	test_flag ();
	test_flag_value ();
	test_optional ();
	test_optional_value ();
	test_precedence ();
	test_repeated ();
	test_multiple ();
	test_multiple_repeated ();
	test_illegal_spec ();
	test_illegal_use ();

	print_result ("test_opts");

	return nbError;
}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdb/errors.h>
#include <elektra/opts.h>

#include "tests.h"

#define PROC_BASE_KEY "proc:/tests/opts"
#define SPEC_BASE_KEY "spec:/tests/opts"

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
		if (elektraGetOpts (ks, args, envp, errorKey) >= 0)                                                                        \
		{                                                                                                                          \
			yield_error ("should have failed");                                                                                \
		}                                                                                                                          \
	}

#ifdef _WIN32
#define ENV_SEP ";"
#else
#define ENV_SEP ":"
#endif

#define checkHelpMessage(errorKey, expected)                                                                                               \
	{                                                                                                                                  \
		char * actual = elektraGetOptsHelpMessage (errorKey, NULL, NULL);                                                          \
		succeed_if_same_string (actual, expected);                                                                                 \
		elektraFree (actual);                                                                                                      \
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
		return expected == NULL;
	}

	const char * actual = keyString (key);
	return expected == NULL ? strlen (actual) == 0 : strcmp (actual, expected) == 0;
}

static bool checkMeta (KeySet * ks, const char * name, const char * meta, const char * expected)
{
	Key * key = ksLookupByName (ks, name, 0);
	if (key == NULL)
	{
		return expected == NULL;
	}

	const Key * metaKey = keyGetMeta (key, meta);
	if (metaKey == NULL)
	{
		return expected == NULL;
	}

	const char * actual = keyString (metaKey);
	return expected == NULL ? strlen (actual) == 0 : strcmp (actual, expected) == 0;
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
	elektraCursor cursor = ksGetCursor (ks);

	ksRewind (ks);
	Key * cur;
	while ((cur = ksNext (ks)) != NULL)
	{
		keySetString (cur, NULL);
		keySetMeta (cur, "array", NULL);
	}

	ksSetCursor (ks, cursor);
}

static void test_simple (void)
{
	KeySet * ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", "APPLE"), KS_END);

	RUN_TEST (ks, NO_ARGS, NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", NULL), "no option failed");
	clearValues (ks);

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

static void test_short_only (void)
{
	KeySet * ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', NULL, NULL), KS_END);

	RUN_TEST (ks, NO_ARGS, NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", NULL), "no option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option (combined) failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_long_only (void)
{
	KeySet * ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 0, "apple", NULL), KS_END);

	RUN_TEST (ks, NO_ARGS, NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", NULL), "no option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple=long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option (combined) failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_env_only (void)
{
	KeySet * ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 0, NULL, "APPLE"), KS_END);

	RUN_TEST (ks, NO_ARGS, NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", NULL), "no option failed");
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
	KeySet * ks = ksNew (1, k, KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag (with arg) failed");
	clearValues (ks);

	Key * errorKey;
	RUN_TEST_ERROR (ks, errorKey, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Unknown short option: -s"),
		    "short flag (with arg, combined) should have failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "long flag failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "long flag (with arg) failed");
	clearValues (ks);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple=long"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "This option cannot have an argument: --apple"),
		    "long flag (with arg, combined) should have failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_flag_value (void)
{
	Key * k = keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL);
	keySetMeta (k, "opt/arg", "none");
	keySetMeta (k, "opt/flagvalue", "set");
	KeySet * ks = ksNew (1, k, KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value (with arg) failed");
	clearValues (ks);

	Key * errorKey;
	RUN_TEST_ERROR (ks, errorKey, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Unknown short option: -s"),
		    "short flag with value (with arg, combined) should have failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "long flag with value failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "long flag with value (with arg) failed");
	clearValues (ks);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple=long"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "This option cannot have an argument: --apple"),
		    "long flag with value (with arg, combined) should have failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_optional (void)
{
	Key * k = keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL);
	keySetMeta (k, "opt/arg", "optional");
	KeySet * ks = ksNew (1, k, KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag (with arg) failed");
	clearValues (ks);

	Key * errorKey;
	RUN_TEST_ERROR (ks, errorKey, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Unknown short option: -s"),
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
	KeySet * ks = ksNew (1, k, KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value (with arg) failed");
	clearValues (ks);

	Key * errorKey;
	RUN_TEST_ERROR (ks, errorKey, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Unknown short option: -s"),
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
	KeySet * ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", "APPLE"), KS_END);

	RUN_TEST (ks, ARGS ("--apple=long", "-ashort"), ENVP ("APPLE=env"));
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option didn't take precedence");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple=long"), ENVP ("APPLE=env"));
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option didn't take precedence");
	clearValues (ks);

	ksDel (ks);
}

static void test_repeated (void)
{
	KeySet * ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple/#", 'a', "apple", "APPLE"), KS_END);

	RUN_TEST (ks, ARGS ("-a", "short0", "-ashort1", "-a", "short2"), NO_ENVP);
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/apple", "array", "#2"), "short repeated failed (wrong count)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#0", "short0"), "short repeated failed (#0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#1", "short1"), "short repeated failed (#1)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#2", "short2"), "short repeated failed (#2)");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long0", "--apple=long1", "--apple", "long2"), NO_ENVP);
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/apple", "array", "#2"), "long repeated failed (wrong count)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#0", "long0"), "long repeated failed (#0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#1", "long1"), "long repeated failed (#1)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#2", "long2"), "long repeated failed (#2)");
	clearValues (ks);

	RUN_TEST (ks, NO_ARGS, ENVP ("APPLE=env0" ENV_SEP "env1" ENV_SEP "env2"));
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/apple", "array", "#2"), "env-var repeated failed (wrong count)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#0", "env0"), "env-var repeated failed (#0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#1", "env1"), "env-var repeated failed (#1)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#2", "env2"), "env-var repeated failed (#2)");
	clearValues (ks);

	ksDel (ks);
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
	KeySet * ks = ksNew (1, k, KS_END);

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

static void test_precedence_repeated (void)
{
	KeySet * ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple/#", 'a', "apple", "APPLE"), KS_END);

	RUN_TEST (ks, ARGS ("--apple=long1", "-a", "short0", "-a", "short1", "--apple", "long0", "--apple", "long2", "-ashort2"),
		  ENVP ("APPLE=env0" ENV_SEP "env1" ENV_SEP "env2"));
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/apple", "array", "#2"), "short repeated failed (wrong count), should take precedence");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#0", "short0"), "short repeated failed (#0), should take precedence");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#1", "short1"), "short repeated failed (#1), should take precedence");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#2", "short2"), "short repeated failed (#2), should take precedence");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long0", "--apple=long1", "--apple", "long2"), ENVP ("APPLE=env0" ENV_SEP "env1" ENV_SEP "env2"));
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/apple", "array", "#2"), "long repeated failed (wrong count), should take precedence");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#0", "long0"), "long repeated failed (#0), should take precedence");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#1", "long1"), "long repeated failed (#1), should take precedence");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple/#2", "long2"), "long repeated failed (#2), should take precedence");
	clearValues (ks);

	ksDel (ks);
}

static void test_illegal_spec (void)
{
	// ---
	// illegal flagvalue
	// ---

	Key * k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "opt", "a");
	keySetMeta (k, "opt/flagvalue", "set");
	KeySet * ks = ksNew (1, k, KS_END);

	Key * errorKey;
	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The flagvalue metadata can only be used, if the opt/arg metadata is set to 'none' or 'optional'. "
				"(key: " SPEC_BASE_KEY "/apple)"),
		    "flagvalue should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// duplicate option (short)
	// ---

	ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', NULL, NULL), keyWithOpt (SPEC_BASE_KEY "/banana", 'a', NULL, NULL), KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The option '-a' has already been specified for the key '" SPEC_BASE_KEY
				"/apple'. Additional key: " SPEC_BASE_KEY "/banana"),
		    "duplicate short option should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// duplicate option (long)
	// ---

	ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 0, "apple", NULL), keyWithOpt (SPEC_BASE_KEY "/banana", 0, "apple", NULL),
		    KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The option '--apple' has already been specified for the key '" SPEC_BASE_KEY
				"/apple'. Additional key: " SPEC_BASE_KEY "/banana"),
		    "duplicate long option should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// duplicate envrionment variable
	//

	ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 0, NULL, "APPLE"), keyWithOpt (SPEC_BASE_KEY "/banana", 0, NULL, "APPLE"),
		    KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The environment variable 'APPLE' has already been specified for the key '" SPEC_BASE_KEY
				"/apple'. Additional key: " SPEC_BASE_KEY "/banana"),
		    "duplicate env-var option should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// args remaining not array
	// ---

	k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "args", "remaining");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'args=remaining' can only be set on array keys (basename = '#'). Offending key: " SPEC_BASE_KEY "/apple"),
		    "non-array remaining args should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// '-' option
	// ---

	k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "opt", "-");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"Character '-' cannot be used as a short option. It would collide with the "
				"special string '--'. Offending key: " SPEC_BASE_KEY "/apple"),
		    "'-' option should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// 'help' option
	// ---

	k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "opt/long", "help");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"Option 'help' cannot be used as a long option. It would collide with the "
				"help option '--help'. Offending key: " SPEC_BASE_KEY "/apple"),
		    "'help' option should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// args indexed without index
	// ---

	k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "args", "indexed");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'args=indexed' must be accompanied by 'args/index'. Offending key: " SPEC_BASE_KEY "/apple"),
		    "args=indexed without args/index should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// args indexed array
	// ---

	k = keyNew (SPEC_BASE_KEY "/apple/#", KEY_END);
	keySetMeta (k, "args", "indexed");
	keySetMeta (k, "args/index", "0");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'args=indexed' can only be set on non-array keys (basename != '#'). Offending key: " SPEC_BASE_KEY
				"/apple/#"),
		    "args=indexed on an array key should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// args indexed missing index
	// ---

	k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "args", "indexed");
	keySetMeta (k, "args/index", "3");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The values of 'args/index' must be continuous, but index 0 is missing in keys below: " SPEC_BASE_KEY),
		    "args=indexed with non-continuous indicies should be illegal");
	clearValues (ks);
	ksDel (ks);


	// ---
	// args duplicate remaining
	// ---

	k = keyNew (SPEC_BASE_KEY "/apple/#", KEY_END);
	keySetMeta (k, "args", "remaining");
	ks = ksNew (2, k, keyNew (SPEC_BASE_KEY "/banana/#", KEY_META, "args", "remaining", KEY_END), KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'args=remaining' is already used on key '" SPEC_BASE_KEY "/apple/#'. Offending key: " SPEC_BASE_KEY
				"/banana/#"),
		    "args=remaining duplicate should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// args duplicate index
	// ---

	k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "args", "indexed");
	keySetMeta (k, "args/index", "0");
	ks = ksNew (2, k, keyNew (SPEC_BASE_KEY "/banana", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END), KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'args/index=0' is already used by '" SPEC_BASE_KEY "/apple'. Offending Key: " SPEC_BASE_KEY "/banana"),
		    "args=indexed duplicate index should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// command non-empty root meta
	// ---

	k = keyNew (SPEC_BASE_KEY, KEY_END);
	keySetMeta (k, "command", "abc");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"On the parent key 'command' can only be set to an empty string. Offending key: " SPEC_BASE_KEY),
		    "command set to non-empty string on parent key should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// command sub without root
	// ---

	k = keyNew (SPEC_BASE_KEY "/cmd", KEY_END);
	keySetMeta (k, "command", "sub");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'command' can only be used, if it is set on the parent key as well. Offending key: " SPEC_BASE_KEY "/cmd"),
		    "sub-commands without command metakey on parent key should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// command sub without parent
	// ---

	k = keyNew (SPEC_BASE_KEY "/cmd/sub", KEY_END);
	keySetMeta (k, "command", "sub");
	ks = ksNew (2, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END), k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/sub) must have the 'command' metakey set. Offending key: parent doesn't exist"),
		    "sub-commands without direct parent should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// command sub parent without command
	// ---

	k = keyNew (SPEC_BASE_KEY "/cmd/sub", KEY_END);
	keySetMeta (k, "command", "sub");
	ks = ksNew (3, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END), keyNew (SPEC_BASE_KEY "/cmd", KEY_END), k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/sub) must have the 'command' metakey set. Offending key: " SPEC_BASE_KEY "/cmd"),
		    "sub-commands without command metakey on direct parent should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// command short option without parent
	// ---

	k = keyNew (SPEC_BASE_KEY "/cmd/opt", KEY_END);
	keySetMeta (k, "opt", "a");
	ks = ksNew (2, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END), k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/opt) must have the 'command' metakey set. Offending key: parent doesn't exist"),
		    "in sub-command mode short options without command metakey on direct parent should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// command long option without parent
	// ---

	k = keyNew (SPEC_BASE_KEY "/cmd/opt", KEY_END);
	keySetMeta (k, "opt/long", "apple");
	ks = ksNew (2, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END), k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/opt) must have the 'command' metakey set. Offending key: parent doesn't exist"),
		    "in sub-command mode long options without command metakey on direct parent should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// command args remaining without parent
	// ---

	k = keyNew (SPEC_BASE_KEY "/cmd/arg/#", KEY_END);
	keySetMeta (k, "args", "remaining");
	ks = ksNew (2, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END), k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/arg/#) must have the 'command' metakey set. Offending key: parent doesn't exist"),
		    "in sub-command mode args=remaining without command metakey on direct parent should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// command args indexed without parent
	// ---

	k = keyNew (SPEC_BASE_KEY "/cmd/arg", KEY_END);
	keySetMeta (k, "args", "indexed");
	keySetMeta (k, "args/index", "0");
	ks = ksNew (2, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END), k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/arg) must have the 'command' metakey set. Offending key: parent doesn't exist"),
		    "in sub-command mode args=indexed without command metakey on direct parent should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// command sub empty
	// ---

	k = keyNew (SPEC_BASE_KEY "/cmd", KEY_END);
	keySetMeta (k, "command", "");
	ks = ksNew (2, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END), k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'command' must be set to a non-empty string (except on the parent key). Offending key: " SPEC_BASE_KEY
				"/cmd"),
		    "sub-command with empty command metadata should be illegal");
	clearValues (ks);
	ksDel (ks);

	// ---
	// command duplicate sub
	// ---

	k = keyNew (SPEC_BASE_KEY "/cmd2", KEY_END);
	keySetMeta (k, "command", "sub");
	ks = ksNew (3, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END),
		    keyNew (SPEC_BASE_KEY "/cmd", KEY_META, "command", "sub", KEY_END), k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"Duplicate sub-command 'sub'. Offending key: " SPEC_BASE_KEY "/cmd2"),
		    "duplicate sub-commands should be illegal");
	clearValues (ks);
	ksDel (ks);
}

static void test_illegal_use (void)
{
	// ---
	// illegal repeat
	// ---

	KeySet * ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL), KS_END);

	Key * errorKey;
	RUN_TEST_ERROR (ks, errorKey, ARGS ("-ashort0", "-ashort1"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "This option cannot be repeated: -a"),
		    "repeat should be illegal (short)");
	clearValues (ks);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple", "long0", "--apple", "long1"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "This option cannot be repeated: --apple"),
		    "repeat should be illegal (long)");
	clearValues (ks);

	ksDel (ks);

	// ---
	// missing argument
	// ---

	ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL), KS_END);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("-a"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Missing argument for short option: -a"),
		    "missing argument (short) failed");
	clearValues (ks);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Missing argument for long option: --apple"),
		    "missing argument (long) failed");
	clearValues (ks);

	ksDel (ks);

	// ---
	// argument not allowed
	// ---

	Key * k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "opt", "a");
	keySetMeta (k, "opt/long", "apple");
	keySetMeta (k, "opt/arg", "none");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple=short"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "This option cannot have an argument: --apple"),
		    "argument should not be allowed");
	clearValues (ks);

	ksDel (ks);

	// ---
	// multiple repeated
	// ---

	k = keyNew (SPEC_BASE_KEY "/apple/#", KEY_END);
	keySetMeta (k, "opt", "#1");
	keySetMeta (k, "opt/#0", "a");
	keySetMeta (k, "opt/#0/long", "apple");
	keySetMeta (k, "opt/#1", "b");
	keySetMeta (k, "opt/#1/long", "banana");
	keySetMeta (k, "env", "#1");
	keySetMeta (k, "env/#0", "APPLE");
	keySetMeta (k, "env/#1", "BANANA");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("-a", "short0", "-ashort1", "-a", "short2", "-b", "short3", "-bshort4"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The option '-b' cannot be used, because another option has already been used for the key "
				"'" SPEC_BASE_KEY "/apple/#'"),
		    "multiple repeated short options should have failed");
	clearValues (ks);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple", "long0", "--apple=long1", "--apple", "long2", "--banana=long3", "--banana", "long4"),
			NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The option '--banana' cannot be used, because another option has already been used for the key "
				"'" SPEC_BASE_KEY "/apple/#'"),
		    "multiple repeated long options should have failed");
	clearValues (ks);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, ENVP ("APPLE=env0" ENV_SEP "env1" ENV_SEP "env2", "BANANA=env3" ENV_SEP "env4"));
	succeed_if (
		checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
			    "The environment variable 'BANANA' cannot be used, because another variable has already been used for the key "
			    "'" SPEC_BASE_KEY "/apple/#'."),
		"multiple repeated env-vars should have failed");
	clearValues (ks);

	ksDel (ks);

	// ---
	// missing indexed arg
	// ---

	k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "args", "indexed");
	keySetMeta (k, "args/index", "0");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Expected at least 1 non-option arguments, but only got 0"),
		    "missing indexed argument should not be allowed");
	clearValues (ks);

	ksDel (ks);

	// ---
	// unknown sub-command
	// ---

	k = keyNew (SPEC_BASE_KEY, KEY_END);
	keySetMeta (k, "command", "");
	ks = ksNew (1, k, KS_END);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("sub"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Unknown sub-command: sub"),
		    "unknown sub-command should not be allowed");
	clearValues (ks);

	ksDel (ks);
}

static void test_help (void)
{
	// ---
	// no options
	// ---

	KeySet * ks = ksNew (0, KS_END);

	Key * errorKey = keyNew (SPEC_BASE_KEY, KEY_END);

	const char * expectedHelpBase =
		"Usage: prog [OPTION...]\n"
		"\n"
		"OPTIONS\n"
		"  --help                      Print this help message\n";

	succeed_if (elektraGetOpts (ks, ARGS ("--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpBase);
	succeed_if (elektraGetOpts (ks, ARGS ("--help", "long"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpBase);

	keyDel (errorKey);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--help=long"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "This option cannot have an argument: --help"),
		    "long help with value (with arg, combined) should have failed");
	clearValues (ks);

	ksDel (ks);

	// ---
	// with options
	// ---

	const char * expectedHelpOpts =
		"Usage: prog [OPTION...] <param1> <param2> [<other>...]\n"
		"\n"
		"OPTIONS\n"
		"  --help                      Print this help message\n"
		"  -a, -b BANANA, -C, --apple, --banana=BANANA, --cherry=[ARG]\n"
		"                                Apple/Banana/Cherry description\n"
		"  -i, -I, --interactive=[WHEN]prompt according to WHEN: never, once (-I), or always (-i), without WHEN, prompt always\n"
		"  -p ARG                      A pear is not an apple, nor a banana, nor a cherry.\n"
		"\n"
		"PARAMETERS\n"
		"  other...                    Other parameters\n"
		"  param1                      First parameter\n"
		"  param2                      Second parameter\n"
		"\n"
		"ENVIRONMENT VARIABLES\n"
		"  APPLE, BANANA, CHERRY       Apple/Banana/Cherry description\n";

	Key * k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "opt", "#3");
	keySetMeta (k, "opt/#0", "a");
	keySetMeta (k, "opt/#0/long", "apple");
	keySetMeta (k, "opt/#0/arg", "none");
	keySetMeta (k, "opt/#1", "b");
	keySetMeta (k, "opt/#1/long", "banana");
	keySetMeta (k, "opt/#1/arg/help", "BANANA");
	keySetMeta (k, "opt/#2", "C");
	keySetMeta (k, "opt/#2/long", "cherry");
	keySetMeta (k, "opt/#2/arg", "optional");
	keySetMeta (k, "opt/#3", "d");
	keySetMeta (k, "opt/#3/hidden", "1");
	keySetMeta (k, "env", "#2");
	keySetMeta (k, "env/#0", "APPLE");
	keySetMeta (k, "env/#1", "BANANA");
	keySetMeta (k, "env/#2", "CHERRY");
	keySetMeta (k, "description", "Apple/Banana/Cherry description");


	// example from the tutorial
	Key * k2 = keyNew (SPEC_BASE_KEY "/interactive", KEY_END);
	keySetMeta (k2, "description", "prompt according to WHEN: never, once (-I), or always (-i), without WHEN, prompt always");
	keySetMeta (k2, "opt", "#1");
	keySetMeta (k2, "opt/#0", "i");
	keySetMeta (k2, "opt/#0/arg", "optional");
	keySetMeta (k2, "opt/#0/flagvalue", "always");
	keySetMeta (k2, "opt/#0/long", "interactive");
	keySetMeta (k2, "opt/#0/arg/help", "WHEN");
	keySetMeta (k2, "opt/#1", "I");
	keySetMeta (k2, "opt/#1/arg", "none");
	keySetMeta (k2, "opt/#1/flagvalue", "once");

	ks = ksNew (5, k, k2,
		    keyNew (SPEC_BASE_KEY "/pear", KEY_META, "opt", "p", KEY_META, "description",
			    "A pear is not an apple, nor a banana, nor a cherry.", KEY_END),
		    keyNew (SPEC_BASE_KEY "/param1", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_META, "description",
			    "First parameter", KEY_END),
		    keyNew (SPEC_BASE_KEY "/param2", KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_META, "description",
			    "Second parameter", KEY_END),
		    keyNew (SPEC_BASE_KEY "/other/#", KEY_META, "args", "remaining", KEY_META, "description", "Other parameters", KEY_END),
		    keyNew (SPEC_BASE_KEY "/none", KEY_META, "opt", "n", KEY_META, "opt/hidden", "1", KEY_END), KS_END);
	errorKey = keyNew (SPEC_BASE_KEY, KEY_END);

	succeed_if (elektraGetOpts (ks, ARGS ("--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpOpts);
	succeed_if (elektraGetOpts (ks, ARGS ("--help", "long"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpOpts);
	clearValues (ks);
	keyDel (errorKey);

	ksDel (ks);


	// ---
	// with option keys nested deeply
	// --

	const char * expectedHelpMainDeep =
		"Usage: prog [OPTION...] [<dynamic>...]\n"
		"\n"
		"OPTIONS\n"
		"  --help                      Print this help message\n"
		"  -x, --notdirectlybelow      \n"
		"  -v, --version               \n"
		"\n"
		"PARAMETERS\n"
		"  dynamic...                  \n";

	ks = ksNew (10, keyNew (SPEC_BASE_KEY, KEY_END),
		    keyNew (SPEC_BASE_KEY "/printversion", KEY_META, "opt", "v", KEY_META, "opt/long", "version", KEY_META, "opt/arg",
			    "none", KEY_END),
		    keyNew (SPEC_BASE_KEY "/oneleveldown/", KEY_END), // A dummy key that simply creates a hierarchy for notdirectlybelow
		    keyNew (SPEC_BASE_KEY "/oneleveldown/twolevelsdown",
			    KEY_END), // A dummy key that simply creates a hierarchy for notdirectlybelow
		    keyNew (SPEC_BASE_KEY "/oneleveldown/twolevelsdown/notdirectlybelow", KEY_META, "opt", "x", KEY_META, "opt/long",
			    "notdirectlybelow", KEY_META, "opt/arg", "none", KEY_END),
		    keyNew (SPEC_BASE_KEY "/dynamic/#", KEY_META, "args", "remaining", KEY_END), KS_END);
	errorKey = keyNew (SPEC_BASE_KEY, KEY_END);

	succeed_if (elektraGetOpts (ks, ARGS ("--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpMainDeep);
	keyDel (errorKey);

	ksDel (ks);


	// ---
	// with commands
	// --

	const char * expectedHelpMain =
		"Usage: prog [OPTION...] [COMMAND [...]|[<dynamic>...]]\n"
		"\n"
		"OPTIONS\n"
		"  --help                      Print this help message\n"
		"  -v, --version               \n"
		"\n"
		"COMMANDS\n"
		"  get                         \n"
		"  set                         \n"
		"\n"
		"PARAMETERS\n"
		"  dynamic...                  \n";

	const char * expectedHelpGet =
		"Usage: prog get [OPTION...] <keyname>\n"
		"\n"
		"OPTIONS\n"
		"  --help                      Print this help message\n"
		"  -v, --verbose               \n"
		"\n"
		"PARAMETERS\n"
		"  keyname                     \n";

	const char * expectedHelpSet =
		"Usage: prog set [OPTION...] <keyname> <value>\n"
		"\n"
		"OPTIONS\n"
		"  --help                      Print this help message\n"
		"  -v, --verbose               \n"
		"\n"
		"PARAMETERS\n"
		"  keyname                     \n"
		"  value                       \n";

	ks = ksNew (10, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END),
		    keyNew (SPEC_BASE_KEY "/printversion", KEY_META, "opt", "v", KEY_META, "opt/long", "version", KEY_META, "opt/arg",
			    "none", KEY_END),
		    keyNew (SPEC_BASE_KEY "/get", KEY_META, "command", "get", KEY_END),
		    keyNew (SPEC_BASE_KEY "/get/verbose", KEY_META, "opt", "v", KEY_META, "opt/long", "verbose", KEY_META, "opt/arg",
			    "none", KEY_END),
		    keyNew (SPEC_BASE_KEY "/get/keyname", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END),
		    keyNew (SPEC_BASE_KEY "/set1", KEY_META, "command", "set", KEY_END),
		    keyNew (SPEC_BASE_KEY "/set1/verbose", KEY_META, "opt", "v", KEY_META, "opt/long", "verbose", KEY_META, "opt/arg",
			    "none", KEY_END),
		    keyNew (SPEC_BASE_KEY "/set1/keyname", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END),
		    keyNew (SPEC_BASE_KEY "/set1/value", KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_END),
		    keyNew (SPEC_BASE_KEY "/dynamic/#", KEY_META, "args", "remaining", KEY_END), KS_END);
	errorKey = keyNew (SPEC_BASE_KEY, KEY_END);

	succeed_if (elektraGetOpts (ks, ARGS ("--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpMain);
	succeed_if (elektraGetOpts (ks, ARGS ("get", "--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpGet);
	succeed_if (elektraGetOpts (ks, ARGS ("set", "--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpSet);
	keyDel (errorKey);

	ksDel (ks);
}

static void test_stop (void)
{
	KeySet * ks = ksNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", "APPLE"),
			     keyNew (SPEC_BASE_KEY "/rest/#", KEY_META, "args", "remaining", KEY_END), KS_END);

	RUN_TEST (ks, ARGS ("--", "-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", NULL), "should have stopped");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/rest", "array", "#1"), "rest has wrong count");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#0", "-a"), "rest has wrong value (#0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#1", "short"), "rest has wrong value (#1)");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-ashort", "--", "-a", "short2"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "should have stopped after short option");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/rest", "array", "#1"), "rest has wrong count");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#0", "-a"), "rest has wrong value (#0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#1", "short2"), "rest has wrong value (#1)");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long", "--", "-a", "short2"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "hould have stopped after long option");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/rest", "array", "#1"), "rest has wrong count");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#0", "-a"), "rest has wrong value (#0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#1", "short2"), "rest has wrong value (#1)");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--"), ENVP ("APPLE=env"));
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "env"), "env-var failed (stopped options)");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/rest", "array", NULL), "rest has wrong count");
	clearValues (ks);


	Key * errorKey = keyNew ("spec:/tests/opts", KEY_META, "posixly", "1", KEY_END);
	if (elektraGetOpts (ks, ARGS ("-ashort", "other", "-a", "short2"), NO_ENVP, errorKey) != 0)
	{
		yield_error ("error found");
		output_error (errorKey);
	}
	keyDel (errorKey);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "should have stopped after short option");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/rest", "array", "#2"), "rest has wrong count");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#0", "other"), "rest has wrong value (#0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#1", "-a"), "rest has wrong value (#1)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#2", "short2"), "rest has wrong value (#2)");
	clearValues (ks);

	ksDel (ks);
}

static void test_mixed_config (void)
{
	Key * k = keyNew (SPEC_BASE_KEY "/apple", KEY_END);
	keySetMeta (k, "opt", "#1");
	keySetMeta (k, "opt/#0", "a");
	keySetMeta (k, "opt/#0/long", "apple");
	keySetMeta (k, "opt/#0/arg", "none");
	keySetMeta (k, "opt/#1", "b");
	keySetMeta (k, "opt/#1/long", "banana");
	KeySet * ks = ksNew (1, k, KS_END);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "mixed config failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "mixed config failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-b", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "mixed config failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-bshort"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "mixed config failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--banana", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "mixed config failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--banana=long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "mixed config failed");
	clearValues (ks);

	ksDel (ks);
}

static void test_args_remaining (void)
{
	KeySet * ks = ksNew (1, keyNew (SPEC_BASE_KEY "/rest/#", KEY_META, "args", "remaining", KEY_END), KS_END);

	RUN_TEST (ks, ARGS ("short0", "short1", "long0", "long2", "test"), NO_ENVP);
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/rest", "array", "#4"), "args remaining (wrong count)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#0", "short0"), "args remaining (#0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#1", "short1"), "args remaining (#1)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#2", "long0"), "args remaining (#2)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#3", "long2"), "args remaining (#3)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#4", "test"), "args remaining (#4)");
	clearValues (ks);

	RUN_TEST (ks, NO_ARGS, NO_ENVP);
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/rest", "array", NULL), "args remaining (wrong count)");
	clearValues (ks);

	ksDel (ks);
}

static void test_args_indexed (void)
{
	KeySet * ks = ksNew (5, keyNew (SPEC_BASE_KEY "/rest0", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END),
			     keyNew (SPEC_BASE_KEY "/rest1", KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_END),
			     keyNew (SPEC_BASE_KEY "/rest2", KEY_META, "args", "indexed", KEY_META, "args/index", "2", KEY_END),
			     keyNew (SPEC_BASE_KEY "/rest3", KEY_META, "args", "indexed", KEY_META, "args/index", "3", KEY_END),
			     keyNew (SPEC_BASE_KEY "/rest4", KEY_META, "args", "indexed", KEY_META, "args/index", "4", KEY_END), KS_END);

	RUN_TEST (ks, ARGS ("short0", "short1", "long0", "long2", "test"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest0", "short0"), "args indexed (0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest1", "short1"), "args indexed (1)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest2", "long0"), "args indexed (2)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest3", "long2"), "args indexed (3)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest4", "test"), "args indexed (4)");
	clearValues (ks);

	ksDel (ks);
}

static void test_args_indexed_and_remaining (void)
{
	KeySet * ks = ksNew (4, keyNew (SPEC_BASE_KEY "/rest/#", KEY_META, "args", "remaining", KEY_END),
			     keyNew (SPEC_BASE_KEY "/rest0", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END),
			     keyNew (SPEC_BASE_KEY "/rest1", KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_END),
			     keyNew (SPEC_BASE_KEY "/rest2", KEY_META, "args", "indexed", KEY_META, "args/index", "2", KEY_END), KS_END);

	RUN_TEST (ks, ARGS ("short0", "short1", "long0", "long2", "test"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest0", "short0"), "args indexed and remaining (index 0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest1", "short1"), "args indexed and remaining (index 1)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest2", "long0"), "args indexed and remaining (index 2)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#0", "long2"), "args indexed and remaining (remaining #0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#1", "test"), "args indexed and remaining (remaining #1)");
	clearValues (ks);

	ksDel (ks);
}

static void test_commands (void)
{
	KeySet * ks = ksNew (10, keyNew (SPEC_BASE_KEY, KEY_META, "command", "", KEY_END),
			     keyNew (SPEC_BASE_KEY "/printversion", KEY_META, "opt", "v", KEY_META, "opt/long", "version", KEY_META,
				     "opt/arg", "none", KEY_END),
			     keyNew (SPEC_BASE_KEY "/get", KEY_META, "command", "get", KEY_END),
			     keyNew (SPEC_BASE_KEY "/get/verbose", KEY_META, "opt", "v", KEY_META, "opt/long", "verbose", KEY_META,
				     "opt/arg", "none", KEY_END),
			     keyNew (SPEC_BASE_KEY "/get/keyname", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END),
			     keyNew (SPEC_BASE_KEY "/set1", KEY_META, "command", "set", KEY_END),
			     keyNew (SPEC_BASE_KEY "/set1/verbose", KEY_META, "opt", "v", KEY_META, "opt/long", "verbose", KEY_META,
				     "opt/arg", "none", KEY_END),
			     keyNew (SPEC_BASE_KEY "/set1/keyname", KEY_META, "args", "indexed", KEY_META, "args/index", "0", KEY_END),
			     keyNew (SPEC_BASE_KEY "/set1/value", KEY_META, "args", "indexed", KEY_META, "args/index", "1", KEY_END),
			     keyNew (SPEC_BASE_KEY "/dynamic/#", KEY_META, "args", "remaining", KEY_END), KS_END);

	RUN_TEST (ks, ARGS ("-v"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY, ""), "command failed: {kdb} -v");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/printversion", "1"), "command failed: kdb {-v}");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get", ""), "command failed: kdb -v [get]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1", ""), "command failed: kdb -v [set]");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/dynamic", "array", NULL), "command failed: kdb -v [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#0", NULL), "command failed: kdb -v [dynamic]");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-v", "get", "x"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY, "get"), "command failed: {kdb} -v get x");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/printversion", "1"), "command failed: kdb {-v} get x");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get", ""), "command failed: kdb -v {get} x");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get/keyname", "x"), "command failed: kdb -v get {x}");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1", ""), "command failed: kdb -v get x [set]");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/dynamic", "array", NULL), "command failed: kdb -v get x [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#0", NULL), "command failed: kdb -v get x [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#1", NULL), "command failed: kdb -v get x[dynamic]");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("get", "-v", "x"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY, "get"), "command failed: {kdb} get -v x");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get", ""), "command failed: kdb {get} -v x");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get/verbose", "1"), "command failed: kdb get {-v} x");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get/keyname", "x"), "command failed: kdb get -v {x}");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1", ""), "command failed: kdb get -v x [set]");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/dynamic", "array", NULL), "command failed: kdb get -v x [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#0", NULL), "command failed: kdb get -v x [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#1", NULL), "command failed: kdb get -v x [dynamic]");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-v", "get", "-v", "abc"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY, "get"), "command failed: {kdb} -v get -v abc");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/printversion", "1"), "command failed: kdb {-v} get -v abc");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get", ""), "command failed: kdb -v {get} -v abc");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get/verbose", "1"), "command failed: kdb -v get {-v} abc");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get/keyname", "abc"), "command failed: kdb -v get -v {abc}");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1", ""), "command failed: kdb -v get -v abc [set]");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/dynamic", "array", NULL), "command failed: kdb -v get -v abc [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#0", NULL), "command failed: kdb -v get -v [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#1", NULL), "command failed: kdb -v get -v [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#2", NULL), "command failed: kdb -v get -v [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#3", NULL), "command failed: kdb -v get -v [dynamic]");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("set", "get", "-v"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY, "set1"), "command failed: {kdb} set get -v");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1", ""), "command failed: kdb {set} get -v");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1/keyname", "get"), "command failed: kdb set {get} -v");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1/value", "-v"), "command failed: kdb set get {-v}");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get", ""), "command failed: kdb set get -v [get]");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/dynamic", "array", NULL), "command failed: kdb set get -v [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#0", NULL), "command failed: kdb set get -v [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#1", NULL), "command failed: kdb set get -v [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#2", NULL), "command failed: kdb set get -v [dynamic]");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("set", "-v", "a", "b"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY, "set1"), "command failed: {kdb} set -v a b");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1", ""), "command failed: kdb {set} -v a b");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1/verbose", "1"), "command failed: kdb set {-v} a b");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1/keyname", "a"), "command failed: kdb set -v {a} b");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1/value", "b"), "command failed: kdb set -v a {b}");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get", ""), "command failed: kdb set get -v [get]");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/dynamic", "array", NULL), "command failed: kdb set get -v [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#0", NULL), "command failed: kdb set get -v [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#1", NULL), "command failed: kdb set get -v [dynamic]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#2", NULL), "command failed: kdb set get -v [dynamic]");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("abc", "-v", "def"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY, ""), "command failed: {kdb} abc -v def");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/get", ""), "command failed: kdb abc -v def [get]");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/set1", ""), "command failed: kdb abc -v def [set]");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/dynamic", "array", "#2"), "command failed: kdb {abc -v def}");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#0", "abc"), "command failed: kdb {abc} -v def");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#1", "-v"), "command failed: kdb abc {-v} def");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/dynamic/#2", "def"), "command failed: kdb abc -v {def}");
	clearValues (ks);

	ksDel (ks);
}

int main (int argc, char ** argv)
{
	printf (" OPTS   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_simple ();
	test_short_only ();
	test_long_only ();
	test_env_only ();
	test_flag ();
	test_flag_value ();
	test_optional ();
	test_optional_value ();
	test_precedence ();
	test_repeated ();
	test_multiple ();
	test_precedence_repeated ();
	test_illegal_spec ();
	test_illegal_use ();
	test_help ();
	test_stop ();
	test_mixed_config ();
	test_args_remaining ();
	test_args_indexed ();
	test_args_indexed_and_remaining ();
	test_commands ();

	print_result ("test_opts");

	return nbError;
}

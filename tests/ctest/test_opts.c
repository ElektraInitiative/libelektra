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
		ElektraKey * ek = keyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END);                                                                                \
		if (elektraGetOpts (ks, args, envp, ek) != 0)                                                                              \
		{                                                                                                                          \
			yield_error ("error found");                                                                                       \
			output_error (ek);                                                                                                 \
		}                                                                                                                          \
		keyDel (ek);                                                                                                               \
	}

#define RUN_TEST_ERROR(ks, errorKey, args, envp)                                                                                           \
	{                                                                                                                                  \
		errorKey = keyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END);                                                                                \
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

static inline ElektraKey * keyWithOpt (const char * name, const char shortOpt, const char * longOpt, const char * envVar)
{
	return elektraKeyNew (name, ELEKTRA_KEY_META, "opt", (const char[]){ shortOpt, '\0' }, ELEKTRA_KEY_META, "opt/long", longOpt, ELEKTRA_KEY_META, "env", envVar,
		       ELEKTRA_KEY_END);
}

static bool checkValue (ElektraKeyset * ks, const char * name, const char * expected)
{
	ElektraKey * key = elektraKeysetLookupByName (ks, name, 0);
	if (key == NULL)
	{
		return expected == NULL;
	}

	const char * actual = elektraKeyString (key);
	return expected == NULL ? strlen (actual) == 0 : strcmp (actual, expected) == 0;
}

static bool checkMeta (ElektraKeyset * ks, const char * name, const char * meta, const char * expected)
{
	ElektraKey * key = elektraKeysetLookupByName (ks, name, 0);
	if (key == NULL)
	{
		return expected == NULL;
	}

	const ElektraKey * metaKey = elektraKeyGetMeta (key, meta);
	if (metaKey == NULL)
	{
		return expected == NULL;
	}

	const char * actual = elektraKeyString (metaKey);
	return expected == NULL ? strlen (actual) == 0 : strcmp (actual, expected) == 0;
}

static bool checkError (ElektraKey * errorKey, const char * expectedNumber, const char * expectedReason)
{
	const ElektraKey * metaError = elektraKeyGetMeta (errorKey, "error");
	if (metaError == NULL)
	{
		return false;
	}

	const char * actualNumber = elektraKeyString (elektraKeyGetMeta (errorKey, "error/number"));
	const char * actualReason = elektraKeyString (elektraKeyGetMeta (errorKey, "error/reason"));

	bool result = strcmp (actualNumber, expectedNumber) == 0 && strcmp (actualReason, expectedReason) == 0;

	elektraKeyDel (errorKey);

	return result;
}

static void clearValues (ElektraKeyset * ks)
{
	elektraCursor cursor = elektraKeysetGetCursor (ks);

	elektraKeysetRewind (ks);
	ElektraKey * cur;
	while ((cur = elektraKeysetNext (ks)) != NULL)
	{
		elektraKeySetString (cur, NULL);
		elektraKeySetMeta (cur, "array", NULL);
	}

	elektraKeysetSetCursor (ks, cursor);
}

static void test_simple (void)
{
	ElektraKeyset * ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", "APPLE"), ELEKTRA_KS_END);

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

	elektraKeysetDel (ks);
}

static void test_short_only (void)
{
	ElektraKeyset * ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', NULL, NULL), ELEKTRA_KS_END);

	RUN_TEST (ks, NO_ARGS, NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", NULL), "no option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-ashort"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option (combined) failed");
	clearValues (ks);

	elektraKeysetDel (ks);
}

static void test_long_only (void)
{
	ElektraKeyset * ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 0, "apple", NULL), ELEKTRA_KS_END);

	RUN_TEST (ks, NO_ARGS, NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", NULL), "no option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple", "long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple=long"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option (combined) failed");
	clearValues (ks);

	elektraKeysetDel (ks);
}

static void test_env_only (void)
{
	ElektraKeyset * ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 0, NULL, "APPLE"), ELEKTRA_KS_END);

	RUN_TEST (ks, NO_ARGS, NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", NULL), "no option failed");
	clearValues (ks);

	RUN_TEST (ks, NO_ARGS, ENVP ("APPLE=env"));
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "env"), "env-var failed");
	clearValues (ks);

	elektraKeysetDel (ks);
}

static void test_flag (void)
{
	ElektraKey * k = keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL);
	elektraKeySetMeta (k, "opt/arg", "none");
	ElektraKeyset * ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag (with arg) failed");
	clearValues (ks);

	ElektraKey * errorKey;
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

	elektraKeysetDel (ks);
}

static void test_flag_value (void)
{
	ElektraKey * k = keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL);
	elektraKeySetMeta (k, "opt/arg", "none");
	elektraKeySetMeta (k, "opt/flagvalue", "set");
	ElektraKeyset * ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value (with arg) failed");
	clearValues (ks);

	ElektraKey * errorKey;
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

	elektraKeysetDel (ks);
}

static void test_optional (void)
{
	ElektraKey * k = keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL);
	elektraKeySetMeta (k, "opt/arg", "optional");
	ElektraKeyset * ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "1"), "short flag (with arg) failed");
	clearValues (ks);

	ElektraKey * errorKey;
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

	elektraKeysetDel (ks);
}

static void test_optional_value (void)
{
	ElektraKey * k = keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL);
	elektraKeySetMeta (k, "opt/arg", "optional");
	elektraKeySetMeta (k, "opt/flagvalue", "set");
	ElektraKeyset * ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST (ks, ARGS ("-a"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value failed");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("-a", "short"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "set"), "short flag with value (with arg) failed");
	clearValues (ks);

	ElektraKey * errorKey;
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

	elektraKeysetDel (ks);
}

static void test_precedence (void)
{
	ElektraKeyset * ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", "APPLE"), ELEKTRA_KS_END);

	RUN_TEST (ks, ARGS ("--apple=long", "-ashort"), ENVP ("APPLE=env"));
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "short option didn't take precedence");
	clearValues (ks);

	RUN_TEST (ks, ARGS ("--apple=long"), ENVP ("APPLE=env"));
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "long"), "long option didn't take precedence");
	clearValues (ks);

	elektraKeysetDel (ks);
}

static void test_repeated (void)
{
	ElektraKeyset * ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple/#", 'a', "apple", "APPLE"), ELEKTRA_KS_END);

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

	elektraKeysetDel (ks);
}

static void test_multiple (void)
{
	ElektraKey * k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "opt", "#1");
	elektraKeySetMeta (k, "opt/#0", "a");
	elektraKeySetMeta (k, "opt/#0/long", "apple");
	elektraKeySetMeta (k, "opt/#1", "b");
	elektraKeySetMeta (k, "opt/#1/long", "banana");
	elektraKeySetMeta (k, "env", "#1");
	elektraKeySetMeta (k, "env/#0", "APPLE");
	elektraKeySetMeta (k, "env/#1", "BANANA");
	ElektraKeyset * ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

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

	elektraKeysetDel (ks);
}

static void test_precedence_repeated (void)
{
	ElektraKeyset * ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple/#", 'a', "apple", "APPLE"), ELEKTRA_KS_END);

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

	elektraKeysetDel (ks);
}

static void test_illegal_spec (void)
{
	// ---
	// illegal flagvalue
	// ---

	ElektraKey * k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "opt", "a");
	elektraKeySetMeta (k, "opt/flagvalue", "set");
	ElektraKeyset * ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	ElektraKey * errorKey;
	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The flagvalue metadata can only be used, if the opt/arg metadata is set to 'none' or 'optional'. "
				"(key: " SPEC_BASE_KEY "/apple)"),
		    "flagvalue should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// duplicate option (short)
	// ---

	ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', NULL, NULL), keyWithOpt (SPEC_BASE_KEY "/banana", 'a', NULL, NULL), ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The option '-a' has already been specified for the key '" SPEC_BASE_KEY
				"/apple'. Additional key: " SPEC_BASE_KEY "/banana"),
		    "duplicate short option should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// duplicate option (long)
	// ---

	ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 0, "apple", NULL), keyWithOpt (SPEC_BASE_KEY "/banana", 0, "apple", NULL),
		    ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The option '--apple' has already been specified for the key '" SPEC_BASE_KEY
				"/apple'. Additional key: " SPEC_BASE_KEY "/banana"),
		    "duplicate long option should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// duplicate envrionment variable
	//

	ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 0, NULL, "APPLE"), keyWithOpt (SPEC_BASE_KEY "/banana", 0, NULL, "APPLE"),
		    ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The environment variable 'APPLE' has already been specified for the key '" SPEC_BASE_KEY
				"/apple'. Additional key: " SPEC_BASE_KEY "/banana"),
		    "duplicate env-var option should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// args remaining not array
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "args", "remaining");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'args=remaining' can only be set on array keys (basename = '#'). Offending key: " SPEC_BASE_KEY "/apple"),
		    "non-array remaining args should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// '-' option
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "opt", "-");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"Character '-' cannot be used as a short option. It would collide with the "
				"special string '--'. Offending key: " SPEC_BASE_KEY "/apple"),
		    "'-' option should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// 'help' option
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "opt/long", "help");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"Option 'help' cannot be used as a long option. It would collide with the "
				"help option '--help'. Offending key: " SPEC_BASE_KEY "/apple"),
		    "'help' option should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// args indexed without index
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "args", "indexed");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'args=indexed' must be accompanied by 'args/index'. Offending key: " SPEC_BASE_KEY "/apple"),
		    "args=indexed without args/index should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// args indexed array
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/apple/#", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "args", "indexed");
	elektraKeySetMeta (k, "args/index", "0");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'args=indexed' can only be set on non-array keys (basename != '#'). Offending key: " SPEC_BASE_KEY
				"/apple/#"),
		    "args=indexed on an array key should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// args indexed missing index
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "args", "indexed");
	elektraKeySetMeta (k, "args/index", "3");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The values of 'args/index' must be continuous, but index 0 is missing in keys below: " SPEC_BASE_KEY),
		    "args=indexed with non-continuous indicies should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);


	// ---
	// args duplicate remaining
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/apple/#", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "args", "remaining");
	ks = elektraKeysetNew (2, k, elektraKeyNew (SPEC_BASE_KEY "/banana/#", ELEKTRA_KEY_META, "args", "remaining", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'args=remaining' is already used on key '" SPEC_BASE_KEY "/apple/#'. Offending key: " SPEC_BASE_KEY
				"/banana/#"),
		    "args=remaining duplicate should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// args duplicate index
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "args", "indexed");
	elektraKeySetMeta (k, "args/index", "0");
	ks = elektraKeysetNew (2, k, elektraKeyNew (SPEC_BASE_KEY "/banana", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'args/index=0' is already used by '" SPEC_BASE_KEY "/apple'. Offending Key: " SPEC_BASE_KEY "/banana"),
		    "args=indexed duplicate index should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// command non-empty root meta
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "command", "abc");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"On the parent key 'command' can only be set to an empty string. Offending key: " SPEC_BASE_KEY),
		    "command set to non-empty string on parent key should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// command sub without root
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/cmd", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "command", "sub");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'command' can only be used, if it is set on the parent key as well. Offending key: " SPEC_BASE_KEY "/cmd"),
		    "sub-commands without command metakey on parent key should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// command sub without parent
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/cmd/sub", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "command", "sub");
	ks = elektraKeysetNew (2, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END), k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/sub) must have the 'command' metakey set. Offending key: parent doesn't exist"),
		    "sub-commands without direct parent should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// command sub parent without command
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/cmd/sub", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "command", "sub");
	ks = elektraKeysetNew (3, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END), elektraKeyNew (SPEC_BASE_KEY "/cmd", ELEKTRA_KEY_END), k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/sub) must have the 'command' metakey set. Offending key: " SPEC_BASE_KEY "/cmd"),
		    "sub-commands without command metakey on direct parent should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// command short option without parent
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/cmd/opt", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "opt", "a");
	ks = elektraKeysetNew (2, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END), k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/opt) must have the 'command' metakey set. Offending key: parent doesn't exist"),
		    "in sub-command mode short options without command metakey on direct parent should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// command long option without parent
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/cmd/opt", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "opt/long", "apple");
	ks = elektraKeysetNew (2, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END), k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/opt) must have the 'command' metakey set. Offending key: parent doesn't exist"),
		    "in sub-command mode long options without command metakey on direct parent should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// command args remaining without parent
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/cmd/arg/#", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "args", "remaining");
	ks = elektraKeysetNew (2, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END), k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/arg/#) must have the 'command' metakey set. Offending key: parent doesn't exist"),
		    "in sub-command mode args=remaining without command metakey on direct parent should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// command args indexed without parent
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/cmd/arg", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "args", "indexed");
	elektraKeySetMeta (k, "args/index", "0");
	ks = elektraKeysetNew (2, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END), k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"The parent of this key (" SPEC_BASE_KEY
				"/cmd/arg) must have the 'command' metakey set. Offending key: parent doesn't exist"),
		    "in sub-command mode args=indexed without command metakey on direct parent should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// command sub empty
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/cmd", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "command", "");
	ks = elektraKeysetNew (2, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END), k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"'command' must be set to a non-empty string (except on the parent key). Offending key: " SPEC_BASE_KEY
				"/cmd"),
		    "sub-command with empty command metadata should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);

	// ---
	// command duplicate sub
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/cmd2", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "command", "sub");
	ks = elektraKeysetNew (3, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/cmd", ELEKTRA_KEY_META, "command", "sub", ELEKTRA_KEY_END), k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC,
				"Duplicate sub-command 'sub'. Offending key: " SPEC_BASE_KEY "/cmd2"),
		    "duplicate sub-commands should be illegal");
	clearValues (ks);
	elektraKeysetDel (ks);
}

static void test_illegal_use (void)
{
	// ---
	// illegal repeat
	// ---

	ElektraKeyset * ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL), ELEKTRA_KS_END);

	ElektraKey * errorKey;
	RUN_TEST_ERROR (ks, errorKey, ARGS ("-ashort0", "-ashort1"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "This option cannot be repeated: -a"),
		    "repeat should be illegal (short)");
	clearValues (ks);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple", "long0", "--apple", "long1"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "This option cannot be repeated: --apple"),
		    "repeat should be illegal (long)");
	clearValues (ks);

	elektraKeysetDel (ks);

	// ---
	// missing argument
	// ---

	ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", NULL), ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("-a"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Missing argument for short option: -a"),
		    "missing argument (short) failed");
	clearValues (ks);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Missing argument for long option: --apple"),
		    "missing argument (long) failed");
	clearValues (ks);

	elektraKeysetDel (ks);

	// ---
	// argument not allowed
	// ---

	ElektraKey * k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "opt", "a");
	elektraKeySetMeta (k, "opt/long", "apple");
	elektraKeySetMeta (k, "opt/arg", "none");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--apple=short"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "This option cannot have an argument: --apple"),
		    "argument should not be allowed");
	clearValues (ks);

	elektraKeysetDel (ks);

	// ---
	// multiple repeated
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/apple/#", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "opt", "#1");
	elektraKeySetMeta (k, "opt/#0", "a");
	elektraKeySetMeta (k, "opt/#0/long", "apple");
	elektraKeySetMeta (k, "opt/#1", "b");
	elektraKeySetMeta (k, "opt/#1/long", "banana");
	elektraKeySetMeta (k, "env", "#1");
	elektraKeySetMeta (k, "env/#0", "APPLE");
	elektraKeySetMeta (k, "env/#1", "BANANA");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

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

	elektraKeysetDel (ks);

	// ---
	// missing indexed arg
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "args", "indexed");
	elektraKeySetMeta (k, "args/index", "0");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, NO_ARGS, NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Expected at least 1 non-option arguments, but only got 0"),
		    "missing indexed argument should not be allowed");
	clearValues (ks);

	elektraKeysetDel (ks);

	// ---
	// unknown sub-command
	// ---

	k = elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "command", "");
	ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("sub"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "Unknown sub-command: sub"),
		    "unknown sub-command should not be allowed");
	clearValues (ks);

	elektraKeysetDel (ks);
}

static void test_help (void)
{
	// ---
	// no options
	// ---

	ElektraKeyset * ks = elektraKeysetNew (0, ELEKTRA_KS_END);

	ElektraKey * errorKey = elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END);

	const char * expectedHelpBase =
		"Usage: prog [OPTION...]\n"
		"\n"
		"OPTIONS\n"
		"  --help                      Print this help message\n";

	succeed_if (elektraGetOpts (ks, ARGS ("--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpBase);
	succeed_if (elektraGetOpts (ks, ARGS ("--help", "long"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpBase);

	elektraKeyDel (errorKey);

	RUN_TEST_ERROR (ks, errorKey, ARGS ("--help=long"), NO_ENVP);
	succeed_if (checkError (errorKey, ELEKTRA_ERROR_VALIDATION_SEMANTIC, "This option cannot have an argument: --help"),
		    "long help with value (with arg, combined) should have failed");
	clearValues (ks);

	elektraKeysetDel (ks);

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
		"  -p ARG                      A pear is not an apple, nor a banana, nor a cherry.\n"
		"\n"
		"PARAMETERS\n"
		"  other...                    Other parameters\n"
		"  param1                      First parameter\n"
		"  param2                      Second parameter\n"
		"\n"
		"ENVIRONMENT VARIABLES\n"
		"  APPLE, BANANA, CHERRY       Apple/Banana/Cherry description\n";

	ElektraKey * k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "opt", "#3");
	elektraKeySetMeta (k, "opt/#0", "a");
	elektraKeySetMeta (k, "opt/#0/long", "apple");
	elektraKeySetMeta (k, "opt/#0/arg", "none");
	elektraKeySetMeta (k, "opt/#1", "b");
	elektraKeySetMeta (k, "opt/#1/long", "banana");
	elektraKeySetMeta (k, "opt/#1/arg/help", "BANANA");
	elektraKeySetMeta (k, "opt/#2", "C");
	elektraKeySetMeta (k, "opt/#2/long", "cherry");
	elektraKeySetMeta (k, "opt/#2/arg", "optional");
	elektraKeySetMeta (k, "opt/#3", "d");
	elektraKeySetMeta (k, "opt/#3/hidden", "1");
	elektraKeySetMeta (k, "env", "#2");
	elektraKeySetMeta (k, "env/#0", "APPLE");
	elektraKeySetMeta (k, "env/#1", "BANANA");
	elektraKeySetMeta (k, "env/#2", "CHERRY");
	elektraKeySetMeta (k, "description", "Apple/Banana/Cherry description");
	ks = elektraKeysetNew (4, k,
		    elektraKeyNew (SPEC_BASE_KEY "/pear", ELEKTRA_KEY_META, "opt", "p", ELEKTRA_KEY_META, "description",
			    "A pear is not an apple, nor a banana, nor a cherry.", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/param1", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_META, "description",
			    "First parameter", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/param2", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "1", ELEKTRA_KEY_META, "description",
			    "Second parameter", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/other/#", ELEKTRA_KEY_META, "args", "remaining", ELEKTRA_KEY_META, "description", "Other parameters", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/none", ELEKTRA_KEY_META, "opt", "n", ELEKTRA_KEY_META, "opt/hidden", "1", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	errorKey = elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END);

	succeed_if (elektraGetOpts (ks, ARGS ("--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpOpts);
	succeed_if (elektraGetOpts (ks, ARGS ("--help", "long"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpOpts);
	clearValues (ks);
	elektraKeyDel (errorKey);

	elektraKeysetDel (ks);


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

	ks = elektraKeysetNew (10, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/printversion", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META, "opt/long", "version", ELEKTRA_KEY_META, "opt/arg",
			    "none", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/oneleveldown/", ELEKTRA_KEY_END), // A dummy key that simply creates a hierarchy for notdirectlybelow
		    elektraKeyNew (SPEC_BASE_KEY "/oneleveldown/twolevelsdown",
			    ELEKTRA_KEY_END), // A dummy key that simply creates a hierarchy for notdirectlybelow
		    elektraKeyNew (SPEC_BASE_KEY "/oneleveldown/twolevelsdown/notdirectlybelow", ELEKTRA_KEY_META, "opt", "x", ELEKTRA_KEY_META, "opt/long",
			    "notdirectlybelow", ELEKTRA_KEY_META, "opt/arg", "none", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/dynamic/#", ELEKTRA_KEY_META, "args", "remaining", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	errorKey = elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END);

	succeed_if (elektraGetOpts (ks, ARGS ("--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpMainDeep);
	elektraKeyDel (errorKey);

	elektraKeysetDel (ks);


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

	ks = elektraKeysetNew (10, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/printversion", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META, "opt/long", "version", ELEKTRA_KEY_META, "opt/arg",
			    "none", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/get", ELEKTRA_KEY_META, "command", "get", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/get/verbose", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META, "opt/long", "verbose", ELEKTRA_KEY_META, "opt/arg",
			    "none", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/get/keyname", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/set1", ELEKTRA_KEY_META, "command", "set", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/set1/verbose", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META, "opt/long", "verbose", ELEKTRA_KEY_META, "opt/arg",
			    "none", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/set1/keyname", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/set1/value", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "1", ELEKTRA_KEY_END),
		    elektraKeyNew (SPEC_BASE_KEY "/dynamic/#", ELEKTRA_KEY_META, "args", "remaining", ELEKTRA_KEY_END), ELEKTRA_KS_END);
	errorKey = elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_END);

	succeed_if (elektraGetOpts (ks, ARGS ("--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpMain);
	succeed_if (elektraGetOpts (ks, ARGS ("get", "--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpGet);
	succeed_if (elektraGetOpts (ks, ARGS ("set", "--help"), NO_ENVP, errorKey) == 1, "help not generated");
	checkHelpMessage (errorKey, expectedHelpSet);
	elektraKeyDel (errorKey);

	elektraKeysetDel (ks);
}

static void test_stop (void)
{
	ElektraKeyset * ks = elektraKeysetNew (1, keyWithOpt (SPEC_BASE_KEY "/apple", 'a', "apple", "APPLE"),
			     elektraKeyNew (SPEC_BASE_KEY "/rest/#", ELEKTRA_KEY_META, "args", "remaining", ELEKTRA_KEY_END), ELEKTRA_KS_END);

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


	ElektraKey * errorKey = elektraKeyNew ("spec:/tests/opts", ELEKTRA_KEY_META, "posixly", "1", ELEKTRA_KEY_END);
	if (elektraGetOpts (ks, ARGS ("-ashort", "other", "-a", "short2"), NO_ENVP, errorKey) != 0)
	{
		yield_error ("error found");
		output_error (errorKey);
	}
	elektraKeyDel (errorKey);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/apple", "short"), "should have stopped after short option");
	succeed_if (checkMeta (ks, PROC_BASE_KEY "/rest", "array", "#2"), "rest has wrong count");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#0", "other"), "rest has wrong value (#0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#1", "-a"), "rest has wrong value (#1)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#2", "short2"), "rest has wrong value (#2)");
	clearValues (ks);

	elektraKeysetDel (ks);
}

static void test_mixed_config (void)
{
	ElektraKey * k = elektraKeyNew (SPEC_BASE_KEY "/apple", ELEKTRA_KEY_END);
	elektraKeySetMeta (k, "opt", "#1");
	elektraKeySetMeta (k, "opt/#0", "a");
	elektraKeySetMeta (k, "opt/#0/long", "apple");
	elektraKeySetMeta (k, "opt/#0/arg", "none");
	elektraKeySetMeta (k, "opt/#1", "b");
	elektraKeySetMeta (k, "opt/#1/long", "banana");
	ElektraKeyset * ks = elektraKeysetNew (1, k, ELEKTRA_KS_END);

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

	elektraKeysetDel (ks);
}

static void test_args_remaining (void)
{
	ElektraKeyset * ks = elektraKeysetNew (1, elektraKeyNew (SPEC_BASE_KEY "/rest/#", ELEKTRA_KEY_META, "args", "remaining", ELEKTRA_KEY_END), ELEKTRA_KS_END);

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

	elektraKeysetDel (ks);
}

static void test_args_indexed (void)
{
	ElektraKeyset * ks = elektraKeysetNew (5, elektraKeyNew (SPEC_BASE_KEY "/rest0", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/rest1", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "1", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/rest2", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "2", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/rest3", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "3", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/rest4", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "4", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	RUN_TEST (ks, ARGS ("short0", "short1", "long0", "long2", "test"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest0", "short0"), "args indexed (0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest1", "short1"), "args indexed (1)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest2", "long0"), "args indexed (2)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest3", "long2"), "args indexed (3)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest4", "test"), "args indexed (4)");
	clearValues (ks);

	elektraKeysetDel (ks);
}

static void test_args_indexed_and_remaining (void)
{
	ElektraKeyset * ks = elektraKeysetNew (4, elektraKeyNew (SPEC_BASE_KEY "/rest/#", ELEKTRA_KEY_META, "args", "remaining", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/rest0", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/rest1", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "1", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/rest2", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "2", ELEKTRA_KEY_END), ELEKTRA_KS_END);

	RUN_TEST (ks, ARGS ("short0", "short1", "long0", "long2", "test"), NO_ENVP);
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest0", "short0"), "args indexed and remaining (index 0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest1", "short1"), "args indexed and remaining (index 1)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest2", "long0"), "args indexed and remaining (index 2)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#0", "long2"), "args indexed and remaining (remaining #0)");
	succeed_if (checkValue (ks, PROC_BASE_KEY "/rest/#1", "test"), "args indexed and remaining (remaining #1)");
	clearValues (ks);

	elektraKeysetDel (ks);
}

static void test_commands (void)
{
	ElektraKeyset * ks = elektraKeysetNew (10, elektraKeyNew (SPEC_BASE_KEY, ELEKTRA_KEY_META, "command", "", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/printversion", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META, "opt/long", "version", ELEKTRA_KEY_META,
				     "opt/arg", "none", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/get", ELEKTRA_KEY_META, "command", "get", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/get/verbose", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META, "opt/long", "verbose", ELEKTRA_KEY_META,
				     "opt/arg", "none", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/get/keyname", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/set1", ELEKTRA_KEY_META, "command", "set", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/set1/verbose", ELEKTRA_KEY_META, "opt", "v", ELEKTRA_KEY_META, "opt/long", "verbose", ELEKTRA_KEY_META,
				     "opt/arg", "none", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/set1/keyname", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "0", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/set1/value", ELEKTRA_KEY_META, "args", "indexed", ELEKTRA_KEY_META, "args/index", "1", ELEKTRA_KEY_END),
			     elektraKeyNew (SPEC_BASE_KEY "/dynamic/#", ELEKTRA_KEY_META, "args", "remaining", ELEKTRA_KEY_END), ELEKTRA_KS_END);

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

	elektraKeysetDel (ks);
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

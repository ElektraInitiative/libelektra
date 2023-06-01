/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/ease/globbing.h>

#include "tests.h"

#define BASE_KEY "user:/tests/globbing"

#define should_match(k, p) succeed_if (check_key (k, p) == 0, p " does not match " k)
#define should_not_match(KEY_NAME, PATTERN)                                                                                                \
	succeed_if (check_key (KEY_NAME, PATTERN) == ELEKTRA_GLOB_NOMATCH, PATTERN " should not match " KEY_NAME)

static int check_key (const char * keyname, const char * pattern)
{
	Key * k = keyNew (keyname, KEY_END);
	int rc = elektraKeyGlob (k, pattern);
	keyDel (k);
	return rc;
}

static void test_star (void)
{
	printf ("star\n");

	should_match (BASE_KEY "/key", BASE_KEY "/*");
	should_match (BASE_KEY "/longkey123__31", BASE_KEY "/*");
	should_match (BASE_KEY "/1231412", BASE_KEY "/*");
	should_match (BASE_KEY "/#1231231", BASE_KEY "/*");
	should_match (BASE_KEY "/#___1234", BASE_KEY "/*");
	should_match (BASE_KEY "/????aased12355", BASE_KEY "/*");
	should_match (BASE_KEY "/***", BASE_KEY "/*");
	should_match (BASE_KEY "/*", BASE_KEY "/*");
	should_match (BASE_KEY "/abc*", BASE_KEY "/*");
	should_match (BASE_KEY "/keyabc", BASE_KEY "/*abc");
	should_match (BASE_KEY "/abckey", BASE_KEY "/*");
	should_match (BASE_KEY "/abckeyabc", BASE_KEY "/*");
	should_match (BASE_KEY "/abcabc", BASE_KEY "/*");
	should_match (BASE_KEY "/abcdabc", BASE_KEY "/*");
	should_match (BASE_KEY "/abc/d/abc", BASE_KEY "/abc/*/abc");
	should_match (BASE_KEY "/d/abc", BASE_KEY "/*/abc");

	should_not_match (BASE_KEY "/", BASE_KEY "/*");
	should_not_match (BASE_KEY "/abc/def", BASE_KEY "/*");
}

static void test_question_mark (void)
{
	printf ("question mark\n");

	should_match (BASE_KEY "/k", BASE_KEY "/?");
	should_match (BASE_KEY "/?", BASE_KEY "/?");
	should_match (BASE_KEY "/key", BASE_KEY "/k?y");
	should_match (BASE_KEY "/key", BASE_KEY "/???");

	should_not_match (BASE_KEY "//", BASE_KEY "/?");
	should_not_match (BASE_KEY "/key", BASE_KEY "/?");
}

static void test_underscore (void)
{
	printf ("underscore\n");

	should_match (BASE_KEY "/key", BASE_KEY "/_");
	should_match (BASE_KEY "/longkey123__31", BASE_KEY "/_");
	should_match (BASE_KEY "/1231412", BASE_KEY "/_");
	should_match (BASE_KEY "/\\#1231231", BASE_KEY "/_");
	should_match (BASE_KEY "/#__1234", BASE_KEY "/_");
	should_match (BASE_KEY "/????aased12355", BASE_KEY "/_");
	should_match (BASE_KEY "/***", BASE_KEY "/_");
	should_match (BASE_KEY "/_", BASE_KEY "/_");
	should_match (BASE_KEY "/abc_", BASE_KEY "/_");
	should_match (BASE_KEY "/abckey", BASE_KEY "/_");
	should_match (BASE_KEY "/abckeyabc", BASE_KEY "/_");
	should_match (BASE_KEY "/abcabc", BASE_KEY "/_");
	should_match (BASE_KEY "/abcdabc", BASE_KEY "/_");
	should_match (BASE_KEY "/abc/d/abc", BASE_KEY "/abc/_/abc");
	should_match (BASE_KEY "/d/abc", BASE_KEY "/_/abc");

	should_not_match (BASE_KEY "/", BASE_KEY "/_");
	should_not_match (BASE_KEY "/abc/def", BASE_KEY "/_");
	should_not_match (BASE_KEY "/keyabc", BASE_KEY "/_abc");
	should_not_match (BASE_KEY "/#___1234", BASE_KEY "/_");
}

static void test_hash (void)
{
	printf ("hash\n");

	should_match (BASE_KEY "/#___1234", BASE_KEY "/#");
	should_match (BASE_KEY "/#___1234/adef", BASE_KEY "/#/adef");

	should_not_match (BASE_KEY "/abc/def", BASE_KEY "/#");
	should_not_match (BASE_KEY "/", BASE_KEY "/#");
	should_not_match (BASE_KEY "/key", BASE_KEY "/#");
	should_not_match (BASE_KEY "/longkey123__31", BASE_KEY "/#");
	should_not_match (BASE_KEY "/1231412", BASE_KEY "/#");
	should_not_match (BASE_KEY "/\\#1231231", BASE_KEY "/#");
	should_not_match (BASE_KEY "/#__1234", BASE_KEY "/#");
	should_not_match (BASE_KEY "/????aased12355", BASE_KEY "/#");
	should_not_match (BASE_KEY "/***", BASE_KEY "/#");
	should_not_match (BASE_KEY "/#", BASE_KEY "/#");
	should_not_match (BASE_KEY "/abc_", BASE_KEY "/#");
	should_not_match (BASE_KEY "/abckey", BASE_KEY "/#");
	should_not_match (BASE_KEY "/abckeyabc", BASE_KEY "/#");
	should_not_match (BASE_KEY "/abcabc", BASE_KEY "/#");
	should_not_match (BASE_KEY "/abcdabc", BASE_KEY "/#");
	should_not_match (BASE_KEY "/abc/d/abc", BASE_KEY "/abc/#/abc");
	should_not_match (BASE_KEY "/d/abc", BASE_KEY "/#/abc");
	should_not_match (BASE_KEY "/keyabc", BASE_KEY "/#abc");
}

static void test_prefix (void)
{
	printf ("prefix\n");

	should_match (BASE_KEY "", BASE_KEY "/__");
	should_match (BASE_KEY "/key", BASE_KEY "/__");
	should_match (BASE_KEY "/key/subkey", BASE_KEY "/__");
	should_match (BASE_KEY "/key/sub/subkey", BASE_KEY "/__");
	should_match (BASE_KEY "/key/door", BASE_KEY "/key/__");
	should_match (BASE_KEY "/__/key", BASE_KEY "/__/key"); // should treat __ as literal

	should_not_match (BASE_KEY "/room/door/key", BASE_KEY "/__/key");
	should_not_match (BASE_KEY "/door/key", BASE_KEY "/key/__");
}

static void test_keyset (void)
{
	printf ("keyset");

	KeySet * test = ksNew (4, keyNew (BASE_KEY "/yes/a", KEY_END), keyNew (BASE_KEY "/yes/b", KEY_END),
			       keyNew (BASE_KEY "/no/a", KEY_END), keyNew (BASE_KEY "/no/b", KEY_END), KS_END);

	KeySet * expected = ksNew (2, keyNew (BASE_KEY "/yes/a", KEY_END), keyNew (BASE_KEY "/yes/b", KEY_END), KS_END);

	KeySet * actual = ksNew (0, KS_END);
	succeed_if (elektraKsGlob (actual, test, BASE_KEY "/yes/*") == ksGetSize (expected), "wrong number of matching keys");

	ksRewind (expected);
	ksRewind (actual);

	Key * curA = ksNext (actual);
	Key * curE = ksNext (expected);
	while (curA != NULL && curE != NULL)
	{
		succeed_if (keyCmp (curA, curE) == 0, keyName (curE));
		curA = ksNext (actual);
		curE = ksNext (expected);
	}

	succeed_if (curA == NULL && curE == NULL, "not same number of keys");

	ksDel (test);
	ksDel (expected);
	ksDel (actual);
}

int main (int argc, char ** argv)
{
	printf (" GLOBBING   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_star ();
	test_question_mark ();
	test_hash ();
	test_underscore ();
	test_prefix ();
	test_keyset ();

	print_result ("test_globbing");

	return nbError;
}

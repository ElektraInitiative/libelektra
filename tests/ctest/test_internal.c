/**
 * @file
 *
 * @brief Test suite for internal data structures.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include "kdbhelper.h"
#include "kdbprivate.h"
#include <tests_internal.h>

static void test_elektraMalloc (void)
{
	char * buffer = 0;
	buffer = elektraMalloc (50);
	exit_if_fail (buffer, "buffer must not be 0 after allocation");
	elektraRealloc ((void **) &buffer, 100);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");
	elektraRealloc ((void **) &buffer, 20);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");
	elektraFree (buffer);

	buffer = elektraCalloc (50);
	exit_if_fail (buffer, "buffer must not be 0 after allocation");
	for (int i = 0; i < 50; ++i)
	{
		succeed_if (buffer[i] == 0, "elektraCalloc did not initialize buffer with zeros");
	}
	elektraRealloc ((void **) &buffer, 100);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");
	elektraRealloc ((void **) &buffer, 20);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");

	char * dup = elektraStrNDup (buffer, 20);
	exit_if_fail (dup, "could not duplicate buffer");
	elektraFree (buffer);
	buffer = 0;
	for (int i = 0; i < 20; ++i)
	{
		succeed_if (dup[i] == 0, "elektraStrNDup did not correctly copy zero-buffer");
	}
	elektraFree (dup);
}

static void test_elektraStrLen (void)
{
	char charSeq[5];

	printf ("Test elektraStrLen\n");
	for (int i = 1; i < 255; ++i)
	{
		charSeq[0] = '\33';
		charSeq[1] = 'a';
		charSeq[2] = i;	      // 1..254
		charSeq[3] = 256 - i; // 255..2
		charSeq[4] = '\0';

		// printf ("%s %d %d\n", charSeq, elektraStrLen (charSeq), strlen(charSeq));
		succeed_if (elektraStrLen ((char *) charSeq) == 5, "could not deduce correct multichar sequence length");
	}
}

#define TEST_VALIDATE_NAME_OK(NAME, MSG) succeed_if (elektraKeyNameValidate (NAME, "/"), MSG " ok");

#define TEST_VALIDATE_NAME_NOK(NAME, MSG) succeed_if (!elektraKeyNameValidate (NAME, "/"), MSG " not ok");

static void test_elektraKeyNameValidate (void)
{
	printf ("test validate key name\n");

	TEST_VALIDATE_NAME_OK ("normalKey", "normal key");
	TEST_VALIDATE_NAME_OK ("nor\\malKey", "stray escape");
	TEST_VALIDATE_NAME_OK ("nor\\\\malKey", "stray escape");
	TEST_VALIDATE_NAME_OK ("nor\\\\mal\\Key", "stray escape");
	TEST_VALIDATE_NAME_NOK ("danglingKey\\", "dangling escape");
	TEST_VALIDATE_NAME_OK ("escapedEKey\\\\", "escape at end");
	TEST_VALIDATE_NAME_NOK ("danglingKey\\\\\\", "dangling escape");
	TEST_VALIDATE_NAME_OK ("escapedEKey\\\\\\\\", "escape at end");
	TEST_VALIDATE_NAME_NOK ("danglingKey\\\\\\\\\\", "dangling escape");
	TEST_VALIDATE_NAME_OK ("escapedEKey\\\\\\\\\\\\", "escape at end");
	TEST_VALIDATE_NAME_NOK ("danglingKey\\\\\\\\\\\\\\", "dangling escape");
}

#define TEST_ESCAPE_PART_OK(PART, EXPECT)                                                                                                  \
	do                                                                                                                                 \
	{                                                                                                                                  \
		succeed_if (elektraKeyNameEscapePart (PART, &dest) > 0, "escaping failed");                                                \
		succeed_if_same_string (dest, EXPECT);                                                                                     \
	} while (0)

static void test_elektraKeyNameEscapePart (void)
{
	printf ("test escape key name part\n");

	char * dest = NULL;
	TEST_ESCAPE_PART_OK ("abc", "abc");
	TEST_ESCAPE_PART_OK (".", "\\.");
	TEST_ESCAPE_PART_OK ("..", "\\..");
	TEST_ESCAPE_PART_OK ("%", "\\%");
	TEST_ESCAPE_PART_OK ("", "%");
	TEST_ESCAPE_PART_OK ("///", "\\/\\/\\/");
	TEST_ESCAPE_PART_OK ("a/b", "a\\/b");
	TEST_ESCAPE_PART_OK ("a//b", "a\\/\\/b");
	TEST_ESCAPE_PART_OK ("a/./b", "a\\/.\\/b");
	TEST_ESCAPE_PART_OK ("a/../b", "a\\/..\\/b");
	TEST_ESCAPE_PART_OK ("a/%/b", "a\\/%\\/b");
	TEST_ESCAPE_PART_OK ("a/x/b", "a\\/x\\/b");
	TEST_ESCAPE_PART_OK ("a\\.", "a\\\\.");
	TEST_ESCAPE_PART_OK ("\\.", "\\\\.");
	TEST_ESCAPE_PART_OK ("\\\\.", "\\\\\\\\.");
	TEST_ESCAPE_PART_OK ("\\..", "\\\\..");
	TEST_ESCAPE_PART_OK ("\\\\..", "\\\\\\\\..");
	TEST_ESCAPE_PART_OK ("\\\\\\..", "\\\\\\\\\\\\..");
	TEST_ESCAPE_PART_OK ("/", "\\/");
	TEST_ESCAPE_PART_OK ("\\/", "\\\\\\/");		      // 1 -> 3
	TEST_ESCAPE_PART_OK ("\\\\/", "\\\\\\\\\\/");	      // 2 -> 5
	TEST_ESCAPE_PART_OK ("ab\\\\/", "ab\\\\\\\\\\/");     // 2 -> 5
	TEST_ESCAPE_PART_OK ("ab\\\\/de", "ab\\\\\\\\\\/de"); // 2 -> 5
	TEST_ESCAPE_PART_OK ("\\", "\\\\");		      // 1 -> 2
	TEST_ESCAPE_PART_OK ("\\\\", "\\\\\\\\");	      // 2 -> 4
	TEST_ESCAPE_PART_OK ("\\\\\\", "\\\\\\\\\\\\");	      // 3 -> 6
	elektraFree (dest);
}

static void test_elektraKeyNameUnescape (void)
{
	printf ("test unescape key name \n");

	char * dest = elektraMalloc (3);
	dest[0] = KEY_NS_CASCADING;
	dest[1] = '\0';
	dest[2] = '\0';
	char * p = NULL;

	succeed_if (elektraKeyNameUnescape ("abc", &dest, 2) == 6, "size of unescaping wrong");
	succeed_if_same_string ("abc", dest + 2);

	succeed_if (elektraKeyNameUnescape ("\\\\.", &dest, 2) == 5, "size of unescaping wrong");
	succeed_if_same_string ("\\.", dest + 2);

	succeed_if (elektraKeyNameUnescape ("abc/def", &dest, 2) == 10, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("abc", p);
	p += 4;
	succeed_if_same_string ("def", p);

	succeed_if (elektraKeyNameUnescape ("abc\\/def", &dest, 2) == 10, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("abc/def", p);

	succeed_if (elektraKeyNameUnescape ("abc/%/def", &dest, 2) == 11, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("abc", p);
	p += 4;
	succeed_if_same_string ("", p);
	p += 1;
	succeed_if_same_string ("def", p);

	succeed_if (elektraKeyNameUnescape ("abc/\\%/def", &dest, 2) == 12, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("abc", p);
	p += 4;
	succeed_if_same_string ("%", p);
	p += 2;
	succeed_if_same_string ("def", p);

	succeed_if (elektraKeyNameUnescape ("abc/\\./def", &dest, 2) == 12, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("abc", p);
	p += 4;
	succeed_if_same_string (".", p);
	p += 2;
	succeed_if_same_string ("def", p);

	succeed_if (elektraKeyNameUnescape ("abc/\\../def", &dest, 2) == 13, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("abc", p);
	p += 4;
	succeed_if_same_string ("..", p);
	p += 3;
	succeed_if_same_string ("def", p);

	succeed_if (elektraKeyNameUnescape ("abc/\\\\../def", &dest, 2) == 14, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("abc", p);
	p += 4;
	succeed_if_same_string ("\\..", p);
	p += 4;
	succeed_if_same_string ("def", p);

	succeed_if (elektraKeyNameUnescape ("a\\\\c/\\../d\\\\f", &dest, 2) == 13, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("a\\c", p);
	p += 4;
	succeed_if_same_string ("..", p);
	p += 3;
	succeed_if_same_string ("d\\f", p);

	succeed_if (elektraKeyNameUnescape ("\\\\bc/\\%/\\\\ef", &dest, 2) == 12, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("\\bc", p);
	p += 4;
	succeed_if_same_string ("%", p);
	p += 2;
	succeed_if_same_string ("\\ef", p);

	succeed_if (elektraKeyNameUnescape ("\\\\b/\\%/\\\\e", &dest, 2) == 10, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("\\b", p);
	p += 3;
	succeed_if_same_string ("%", p);
	p += 2;
	succeed_if_same_string ("\\e", p);

	succeed_if (elektraKeyNameUnescape ("\\\\b/\\\\%/\\\\e", &dest, 2) == 11, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("\\b", p);
	p += 3;
	succeed_if_same_string ("\\%", p);
	p += 3;
	succeed_if_same_string ("\\e", p);

	succeed_if (elektraKeyNameUnescape ("a\\/\\/def", &dest, 2) == 9, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("a//def", p);

	succeed_if (elektraKeyNameUnescape ("\\/\\/\\/def", &dest, 2) == 9, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("///def", p);

	succeed_if (elektraKeyNameUnescape ("\\/\\/\\/def", &dest, 2) == 9, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("///def", p);

	succeed_if (elektraKeyNameUnescape ("\\/\\/\\/\\/\\/\\/", &dest, 2) == 9, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("//////", p);

	succeed_if (elektraKeyNameUnescape ("\\/\\/%\\/\\/\\/", &dest, 2) == 9, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("//%///", p);

	succeed_if (elektraKeyNameUnescape ("\\/\\/..\\/\\/", &dest, 2) == 9, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("//..//", p);

	succeed_if (elektraKeyNameUnescape ("bar\\/foo_bar\\/", &dest, 2) == sizeof ("bar/foo_bar/") + 2, "size of unescaping wrong");
	p = dest + 2;
	succeed_if_same_string ("bar/foo_bar/", p);

	elektraFree (dest);
}

static void test_keySetNamespace (void)
{
	printf ("test keySetNamespace\n");

	Key * k = keyNew ("/test/123", KEY_END);
	succeed_if (keySetNamespace (k, KEY_NS_CASCADING) == 10, "new size wrong");
	succeed_if (keyGetNamespace (k) == KEY_NS_CASCADING, "new namespace wrong");
	succeed_if_same_string (keyName (k), "/test/123");

	succeed_if (keySetNamespace (k, KEY_NS_USER) == 15, "new size wrong");
	succeed_if (keyGetNamespace (k) == KEY_NS_USER, "new namespace wrong");
	succeed_if_same_string (keyName (k), "user:/test/123");

	succeed_if (keySetNamespace (k, KEY_NS_SPEC) == 15, "new size wrong");
	succeed_if (keyGetNamespace (k) == KEY_NS_SPEC, "new namespace wrong");
	succeed_if_same_string (keyName (k), "spec:/test/123");

	succeed_if (keySetNamespace (k, KEY_NS_SYSTEM) == 17, "new size wrong");
	succeed_if (keyGetNamespace (k) == KEY_NS_SYSTEM, "new namespace wrong");
	succeed_if_same_string (keyName (k), "system:/test/123");

	keyDel (k);
}

int main (int argc, char ** argv)
{
	printf ("INTERNALS    TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_elektraMalloc ();
	test_elektraStrLen ();
	test_elektraKeyNameValidate ();
	test_elektraKeyNameEscapePart ();
	test_elektraKeyNameUnescape ();
	test_keySetNamespace ();

	printf ("\ntest_internals RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}

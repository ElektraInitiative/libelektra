/*************************************************************************** 
 *           test_internal.c  - Test suite for internal data structures
 *                  -------------------
 *  begin                : Fri 21 Mar 2008
 *  copyright            : (C) 2008 by Markus Raab
 *  email                : elektra@markus-raab.org
 ****************************************************************************/

/***************************************************************************
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the BSD License (revised).                      *
 *                                                                         *
 ***************************************************************************/

#include <tests_internal.h>

static void test_elektraMalloc()
{
	char * buffer=0;
	buffer = elektraMalloc(50);
	exit_if_fail (buffer, "buffer must not be 0 after allocation");
	elektraRealloc((void**)&buffer, 100);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");
	elektraRealloc((void**)&buffer, 20);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");
	elektraFree(buffer);

	buffer = elektraCalloc(50);
	exit_if_fail (buffer, "buffer must not be 0 after allocation");
	for (int i=0; i<50; ++i)
	{
		succeed_if (buffer[i] == 0, "elektraCalloc did not initialize buffer with zeros");
	}
	elektraRealloc((void**)&buffer, 100);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");
	elektraRealloc((void**)&buffer, 20);
	exit_if_fail (buffer, "buffer must not be 0 after reallocation");

	char * dup = elektraStrNDup(buffer, 20);
	exit_if_fail (dup, "could not duplicate buffer");
	elektraFree(buffer);
	buffer = 0;
	for (int i=0; i<20; ++i)
	{
		succeed_if (dup[i] == 0, "elektraStrNDup did not correctly copy zero-buffer");
	}
	elektraFree(dup);
}

static void test_elektraStrLen()
{
	char charSeq [5];

	printf ("Test elektraStrLen\n");
	for (int i=1; i<255; ++i)
	{
		charSeq [0] = '\33';
		charSeq [1] = 'a';
		charSeq [2] = i;     // 1..254
		charSeq [3] = 256-i; // 255..2
		charSeq [4] = '\0';

		// printf ("%s %d %d\n", charSeq, elektraStrLen (charSeq), strlen(charSeq));
		succeed_if(elektraStrLen ((char*)charSeq) == 5, "could not deduce correct multichar sequence length");
	}
}

static void test_elektraValidateKeyNamePart()
{
	printf ("test validate key name part\n");

	succeed_if (elektraValidateKeyNamePart("normalKey"), "key name part without special characters is invalid");
	succeed_if (!elektraValidateKeyNamePart("Icontaina\\"), "unescaped backslash is valid");
	succeed_if (!elektraValidateKeyNamePart("Icontaina/"), "unescaped slash is valid");
	succeed_if (elektraValidateKeyNamePart("Icontaina%"), "unescaped % is invalid")
	succeed_if (elektraValidateKeyNamePart("Icontaina#"), "unescaped # is invalid")
	succeed_if (elektraValidateKeyNamePart("Icontaina."), "unescaped . is invalid")
	succeed_if (elektraValidateKeyNamePart("Icontaina.."), "unescaped .. is invalid")
	succeed_if (!elektraValidateKeyNamePart("\\x"), "invalid escape sequence is valid");
	succeed_if (!elektraValidateKeyNamePart("textbefore\\x"), "invalid escape sequence is valid");
	succeed_if (elektraValidateKeyNamePart("\\\\"), "escaped escape character is invalid");
	succeed_if (elektraValidateKeyNamePart("\\/"), "escaped slash is invalid");
}

static void test_elektraEscapeKeyNamePart()
{
	printf ("test escape key name part\n");

	char dest [2000];
	succeed_if_same_string (elektraEscapeKeyNamePart("abc", dest), "abc");
	succeed_if_same_string (elektraEscapeKeyNamePart(".", dest), "\\.");
	succeed_if_same_string (elektraEscapeKeyNamePart("..", dest), "\\..");
	succeed_if_same_string (elektraEscapeKeyNamePart("%", dest), "\\%");
	succeed_if_same_string (elektraEscapeKeyNamePart("", dest), "%");
	succeed_if_same_string (elektraEscapeKeyNamePart("///", dest), "\\/\\/\\/");
	succeed_if_same_string (elektraEscapeKeyNamePart("a/b", dest), "a\\/b");
	succeed_if_same_string (elektraEscapeKeyNamePart("a//b", dest), "a\\/\\/b");
	succeed_if_same_string (elektraEscapeKeyNamePart("a/./b", dest), "a\\/.\\/b");
	succeed_if_same_string (elektraEscapeKeyNamePart("a/../b", dest), "a\\/..\\/b");
	succeed_if_same_string (elektraEscapeKeyNamePart("a/%/b", dest), "a\\/%\\/b");
	succeed_if_same_string (elektraEscapeKeyNamePart("a/x/b", dest), "a\\/x\\/b");
	succeed_if_same_string (elektraEscapeKeyNamePart("\\", dest), "\\");
	succeed_if_same_string (elektraEscapeKeyNamePart("a\\.", dest), "a\\.");
	succeed_if_same_string (elektraEscapeKeyNamePart("\\.", dest), "\\\\.");
	succeed_if_same_string (elektraEscapeKeyNamePart("\\\\.", dest), "\\\\\\.");
	succeed_if_same_string (elektraEscapeKeyNamePart("\\..", dest), "\\\\..");
	succeed_if_same_string (elektraEscapeKeyNamePart("\\\\..", dest), "\\\\\\..");
	succeed_if_same_string (elektraEscapeKeyNamePart("\\\\\\..", dest), "\\\\\\\\..");
	succeed_if_same_string (elektraEscapeKeyNamePart("/", dest), "\\/");
	succeed_if_same_string (elektraEscapeKeyNamePart("\\/", dest), "\\\\/");
	succeed_if_same_string (elektraEscapeKeyNamePart("\\\\/", dest), "\\\\\\/");
	succeed_if_same_string (elektraEscapeKeyNamePart("ab\\\\/", dest), "ab\\\\\\/");
	succeed_if_same_string (elektraEscapeKeyNamePart("ab\\\\/de", dest), "ab\\\\\\/de");
}

static void test_elektraUnescapeKeyName()
{
	printf ("test unescape key name \n");

	char dest[2000];
	char * p = dest; 

	succeed_if (elektraUnescapeKeyName("abc", dest) == 4, "size of unescaping wrong");
	succeed_if_same_string("abc", dest);

	succeed_if (elektraUnescapeKeyName("\\\\.", dest) == 3, "size of unescaping wrong");
	succeed_if_same_string("\\.", dest);

	succeed_if (elektraUnescapeKeyName("abc/def", dest) == 8, "size of unescaping wrong");
	p = dest; succeed_if_same_string("abc", p);
	p += 4;   succeed_if_same_string("def", p);

	succeed_if(elektraUnescapeKeyName("abc\\/def", dest) == 8, "size of unescaping wrong");
	p = dest; succeed_if_same_string("abc/def", p);

	succeed_if(elektraUnescapeKeyName("abc/%/def", dest) == 9, "size of unescaping wrong");
	p = dest; succeed_if_same_string("abc", p);
	p += 4;   succeed_if_same_string("", p);
	p += 1;   succeed_if_same_string("def", p);

	succeed_if(elektraUnescapeKeyName("abc/\\%/def", dest) == 10, "size of unescaping wrong");
	p = dest; succeed_if_same_string("abc", p);
	p += 4;   succeed_if_same_string("%", p);
	p += 2;   succeed_if_same_string("def", p);

	succeed_if(elektraUnescapeKeyName("abc/\\./def", dest) == 10, "size of unescaping wrong");
	p = dest; succeed_if_same_string("abc", p);
	p += 4;   succeed_if_same_string(".", p);
	p += 2;   succeed_if_same_string("def", p);

	succeed_if(elektraUnescapeKeyName("abc/\\../def", dest) == 11, "size of unescaping wrong");
	p = dest; succeed_if_same_string("abc", p);
	p += 4;   succeed_if_same_string("..", p);
	p += 3;   succeed_if_same_string("def", p);

	succeed_if(elektraUnescapeKeyName("abc/\\\\../def", dest) == 12, "size of unescaping wrong");
	p = dest; succeed_if_same_string("abc", p);
	p += 4;   succeed_if_same_string("\\..", p);
	p += 4;   succeed_if_same_string("def", p);

	succeed_if(elektraUnescapeKeyName("a\\c/\\../d\\f", dest) == 11, "size of unescaping wrong");
	p = dest; succeed_if_same_string("a\\c", p);
	p += 4;   succeed_if_same_string("..", p);
	p += 3;   succeed_if_same_string("d\\f", p);

	succeed_if(elektraUnescapeKeyName("\\bc/\\%/\\ef", dest) == 10, "size of unescaping wrong");
	p = dest; succeed_if_same_string("\\bc", p);
	p += 4;   succeed_if_same_string("%", p);
	p += 2;   succeed_if_same_string("\\ef", p);

	succeed_if(elektraUnescapeKeyName("\\b/\\%/\\e", dest) == 8, "size of unescaping wrong");
	p = dest; succeed_if_same_string("\\b", p);
	p += 3;   succeed_if_same_string("%", p);
	p += 2;   succeed_if_same_string("\\e", p);

	succeed_if(elektraUnescapeKeyName("\\b/\\\\%/\\e", dest) == 9, "size of unescaping wrong");
	p = dest; succeed_if_same_string("\\b", p);
	p += 3;   succeed_if_same_string("\\%", p);
	p += 3;   succeed_if_same_string("\\e", p);

	succeed_if(elektraUnescapeKeyName("a\\/\\/def", dest) == 7, "size of unescaping wrong");
	p = dest; succeed_if_same_string("a//def", p);

	succeed_if(elektraUnescapeKeyName("\\/\\/\\/def", dest) == 7, "size of unescaping wrong");
	p = dest; succeed_if_same_string("///def", p);

	succeed_if(elektraUnescapeKeyName("\\/\\/\\/def", dest) == 7, "size of unescaping wrong");
	p = dest; succeed_if_same_string("///def", p);

	succeed_if(elektraUnescapeKeyName("\\/\\/\\/\\/\\/\\/", dest) == 7, "size of unescaping wrong");
	p = dest; succeed_if_same_string("//////", p);

	succeed_if(elektraUnescapeKeyName("\\/\\/%\\/\\/\\/", dest) == 7, "size of unescaping wrong");
	p = dest; succeed_if_same_string("//%///", p);

	succeed_if(elektraUnescapeKeyName("\\/\\/..\\/\\/", dest) == 7, "size of unescaping wrong");
	p = dest; succeed_if_same_string("//..//", p);

	succeed_if(elektraUnescapeKeyName("bar\\/foo_bar\\/", dest) == sizeof("bar/foo_bar/"), "size of unescaping wrong");
	p = dest; succeed_if_same_string("bar/foo_bar/", p);
}

static void test_keyNameGetOneLevel()
{
	printf ("test keyNameGetOneLevel\n");

	size_t size = 0;
	char buffer [] = "a\\/\\/def";
	char * p = keyNameGetOneLevel(buffer, &size);
	succeed_if (p == buffer, "p not at start of buffer");
	succeed_if (size == sizeof(buffer)-1, "size not set correctly");
}

int main(int argc, char** argv)
{
	printf("INTERNALS    TESTS\n");
	printf("==================\n\n");

	init (argc, argv);

	test_elektraMalloc();
	test_elektraStrLen();
	test_elektraValidateKeyNamePart();
	test_elektraEscapeKeyNamePart();
	test_elektraUnescapeKeyName();
	test_keyNameGetOneLevel();

	printf("\ntest_internals RESULTS: %d test(s) done. %d error(s).\n", nbTest, nbError);

	return nbError;
}


/**
 * @file
 *
 * @brief Test suite for functions used in creating key name data.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <kdbprivate.h>

#include "tests.h"

#define TEST_VALIDATE_OK(name, prefix, usizeOld, csizeNew, usizeNew)                                                                       \
	do                                                                                                                                 \
	{                                                                                                                                  \
		size_t csize = prefix == NULL ? 0 : sizeof (prefix);                                                                       \
		size_t usize = usizeOld;                                                                                                   \
		succeed_if_fmt (elektraKeyNameValidate (name, prefix, &csize, &usize), "'%s' + '%s' SHOULD BE a valid key name",           \
				prefix == NULL ? "" : prefix, name);                                                                       \
		succeed_if_fmt (csize == csizeNew, "'%s' + '%s': canonical size wrong (act != exp): %zu != %zu",                           \
				prefix == NULL ? "" : prefix, name, csize, (size_t) csizeNew);                                             \
		succeed_if_fmt (usize == usizeNew, "'%s' + '%s': unescaped size wrong (act != exp): %zu != %zu",                           \
				prefix == NULL ? "" : prefix, name, usize, (size_t) usizeNew);                                             \
	} while (0)

#define TEST_VALIDATE_ERROR(name, prefix, usizeOld)                                                                                        \
	do                                                                                                                                 \
	{                                                                                                                                  \
		size_t c = prefix == NULL ? 1234 : sizeof (prefix);                                                                        \
		size_t u = usizeOld;                                                                                                       \
		succeed_if_fmt (!elektraKeyNameValidate (name, prefix, &c, &u), "'%s' + '%s' SHOULD NOT BE a valid key name",              \
				prefix == NULL ? "" : prefix, name);                                                                       \
		succeed_if_fmt (c == (prefix == NULL ? 1234 : sizeof (prefix)), "'%s' + '%s': ERROR case MUST NOT change canonical size",  \
				prefix == NULL ? "" : prefix, name);                                                                       \
		succeed_if_fmt (u == usizeOld, "'%s' + '%s': ERROR case MUST NOT change unescaped size", prefix == NULL ? "" : prefix,     \
				name);                                                                                                     \
	} while (0)

static void test_validate (void)
{
	TEST_VALIDATE_OK ("/", NULL, 0, 2, 3);
	TEST_VALIDATE_OK ("proc:/", NULL, 0, 7, 3);
	TEST_VALIDATE_OK ("dir:/", NULL, 0, 6, 3);
	TEST_VALIDATE_OK ("user:/", NULL, 0, 7, 3);
	TEST_VALIDATE_OK ("system:/", NULL, 0, 9, 3);
	TEST_VALIDATE_OK ("spec:/", NULL, 0, 7, 3);
	TEST_VALIDATE_OK ("meta:/", NULL, 0, 7, 3);
	TEST_VALIDATE_OK ("default:/", NULL, 0, 10, 3);

	TEST_VALIDATE_OK ("/a", NULL, 0, 3, 4);
	TEST_VALIDATE_OK ("/ab", NULL, 0, 4, 5);
	TEST_VALIDATE_OK ("/abc", NULL, 0, 5, 6);

	TEST_VALIDATE_OK ("/a", NULL, 123, 3, 4);
	TEST_VALIDATE_OK ("/ab", NULL, 22, 4, 5);
	TEST_VALIDATE_OK ("/abc", NULL, 33, 5, 6);

	TEST_VALIDATE_OK ("/%", NULL, 0, 3, 3);
	TEST_VALIDATE_OK ("/\\%", NULL, 0, 4, 4);
	TEST_VALIDATE_OK ("/\\\\%", NULL, 0, 5, 5);
	TEST_VALIDATE_OK ("/\\\\\\\\%", NULL, 0, 7, 6);

	TEST_VALIDATE_OK ("/\\/", NULL, 0, 4, 4);
	TEST_VALIDATE_OK ("/\\\\/", NULL, 0, 4, 4);
	TEST_VALIDATE_OK ("/\\\\\\/", NULL, 0, 6, 5);
	TEST_VALIDATE_OK ("/\\\\\\\\/", NULL, 0, 6, 5);
	TEST_VALIDATE_OK ("/\\\\\\\\\\/", NULL, 0, 8, 6);

	TEST_VALIDATE_OK ("/\\/", NULL, 0, 4, 4);
	TEST_VALIDATE_OK ("/\\\\", NULL, 0, 4, 4);
	TEST_VALIDATE_OK ("/\\\\\\/", NULL, 0, 6, 5);
	TEST_VALIDATE_OK ("/\\\\\\\\", NULL, 0, 6, 5);
	TEST_VALIDATE_OK ("/\\\\\\\\\\/", NULL, 0, 8, 6);

	TEST_VALIDATE_OK ("/\\//", NULL, 0, 4, 4);
	TEST_VALIDATE_OK ("/\\\\/", NULL, 0, 4, 4);
	TEST_VALIDATE_OK ("/\\\\\\//", NULL, 0, 6, 5);
	TEST_VALIDATE_OK ("/\\\\\\\\/", NULL, 0, 6, 5);
	TEST_VALIDATE_OK ("/\\\\\\\\\\//", NULL, 0, 8, 6);

	TEST_VALIDATE_OK ("user:/\\/", NULL, 0, 9, 4);
	TEST_VALIDATE_OK ("user:/\\\\", NULL, 0, 9, 4);
	TEST_VALIDATE_OK ("user:/\\\\\\/", NULL, 0, 11, 5);
	TEST_VALIDATE_OK ("user:/\\\\\\\\", NULL, 0, 11, 5);
	TEST_VALIDATE_OK ("user:/\\\\\\\\\\/", NULL, 0, 13, 6);

	TEST_VALIDATE_OK ("user:/\\//", NULL, 0, 9, 4);
	TEST_VALIDATE_OK ("user:/\\\\/", NULL, 0, 9, 4);
	TEST_VALIDATE_OK ("user:/\\\\\\//", NULL, 0, 11, 5);
	TEST_VALIDATE_OK ("user:/\\\\\\\\/", NULL, 0, 11, 5);
	TEST_VALIDATE_OK ("user:/\\\\\\\\\\//", NULL, 0, 13, 6);

	TEST_VALIDATE_OK ("user:/tests/plugin/\\/", NULL, 0, 22, 17);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\", NULL, 0, 22, 17);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\/", NULL, 0, 24, 18);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\", NULL, 0, 24, 18);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\\\/", NULL, 0, 26, 19);

	TEST_VALIDATE_OK ("user:/tests/plugin/\\//", NULL, 0, 22, 17);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\/", NULL, 0, 22, 17);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\//", NULL, 0, 24, 18);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\/", NULL, 0, 24, 18);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\\\//", NULL, 0, 26, 19);

	TEST_VALIDATE_OK ("/\\//abc", NULL, 0, 8, 8);
	TEST_VALIDATE_OK ("/\\\\/abc", NULL, 0, 8, 8);
	TEST_VALIDATE_OK ("/\\\\\\//abc", NULL, 0, 10, 9);
	TEST_VALIDATE_OK ("/\\\\\\\\/abc", NULL, 0, 10, 9);
	TEST_VALIDATE_OK ("/\\\\\\\\\\//abc", NULL, 0, 12, 10);

	TEST_VALIDATE_OK ("user:/\\//abc", NULL, 0, 13, 8);
	TEST_VALIDATE_OK ("user:/\\\\/abc", NULL, 0, 13, 8);
	TEST_VALIDATE_OK ("user:/\\\\\\//abc", NULL, 0, 15, 9);
	TEST_VALIDATE_OK ("user:/\\\\\\\\/abc", NULL, 0, 15, 9);
	TEST_VALIDATE_OK ("user:/\\\\\\\\\\//abc", NULL, 0, 17, 10);

	TEST_VALIDATE_OK ("user:/tests/plugin/\\//abc", NULL, 0, 26, 21);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\/abc", NULL, 0, 26, 21);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\//abc", NULL, 0, 28, 22);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\/abc", NULL, 0, 28, 22);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\\\//abc", NULL, 0, 30, 23);

	TEST_VALIDATE_OK ("/\\///", NULL, 0, 4, 4);
	TEST_VALIDATE_OK ("/\\/\\/", NULL, 0, 6, 5);

	TEST_VALIDATE_OK ("/abc/def/ghi", NULL, 0, 13, 14);
	TEST_VALIDATE_OK ("user:/abc/def/ghi", NULL, 0, 18, 14);

	TEST_VALIDATE_OK ("/abc/def/ghi/", NULL, 0, 13, 14);
	TEST_VALIDATE_OK ("user:/abc/def/ghi/", NULL, 0, 18, 14);

	TEST_VALIDATE_OK ("/abc//def////ghi/", NULL, 0, 13, 14);
	TEST_VALIDATE_OK ("user://abc////def/ghi/", NULL, 0, 18, 14);

	TEST_VALIDATE_OK ("/////////", NULL, 0, 2, 3);
	TEST_VALIDATE_OK ("user://///////", NULL, 0, 7, 3);

	TEST_VALIDATE_OK ("/a////////", NULL, 0, 3, 4);
	TEST_VALIDATE_OK ("user:/a////////", NULL, 0, 8, 4);

	TEST_VALIDATE_OK ("/abc/%/def", NULL, 0, 11, 11);
	TEST_VALIDATE_OK ("user:/abc/%/def", NULL, 0, 16, 11);

	TEST_VALIDATE_OK ("/abc/d@ef/ghi", NULL, 0, 14, 15);
	TEST_VALIDATE_OK ("user:/abc/d@ef/ghi", NULL, 0, 19, 15);

	TEST_VALIDATE_OK ("/abc/d%ef/ghi", NULL, 0, 14, 15);
	TEST_VALIDATE_OK ("user:/abc/d%ef/ghi", NULL, 0, 19, 15);

	TEST_VALIDATE_OK ("/abc/.def/ghi", NULL, 0, 14, 15);
	TEST_VALIDATE_OK ("user:/abc/.def/ghi", NULL, 0, 19, 15);

	TEST_VALIDATE_OK ("/abc/./ghi", NULL, 0, 9, 10);
	TEST_VALIDATE_OK ("user:/abc/./ghi", NULL, 0, 14, 10);

	TEST_VALIDATE_OK ("/abc/../ghi", NULL, 0, 5, 6);
	TEST_VALIDATE_OK ("user:/abc/../ghi", NULL, 0, 10, 6);

	TEST_VALIDATE_OK ("/abc/.../ghi", NULL, 0, 13, 14);
	TEST_VALIDATE_OK ("user:/abc/.../ghi", NULL, 0, 18, 14);

	TEST_VALIDATE_OK ("/abc/..../ghi", NULL, 0, 14, 15);
	TEST_VALIDATE_OK ("user:/abc/..../ghi", NULL, 0, 19, 15);

	TEST_VALIDATE_OK ("/abc/#0/ghi", NULL, 0, 12, 13);
	TEST_VALIDATE_OK ("user:/abc/#0/ghi", NULL, 0, 17, 13);

	TEST_VALIDATE_OK ("/abc/#1/ghi", NULL, 0, 12, 13);
	TEST_VALIDATE_OK ("user:/abc/#1/ghi", NULL, 0, 17, 13);

	TEST_VALIDATE_OK ("/abc/#5/ghi", NULL, 0, 12, 13);
	TEST_VALIDATE_OK ("user:/abc/#5/ghi", NULL, 0, 17, 13);

	TEST_VALIDATE_OK ("/abc/#10/ghi", NULL, 0, 14, 15);
	TEST_VALIDATE_OK ("user:/abc/#10/ghi", NULL, 0, 19, 15);

	TEST_VALIDATE_OK ("/abc/#10000/ghi", NULL, 0, 20, 21);
	TEST_VALIDATE_OK ("user:/abc/#10000/ghi", NULL, 0, 25, 21);

	TEST_VALIDATE_OK ("/abc/#9223372036854775807/ghi", NULL, 0, 48, 49);
	TEST_VALIDATE_OK ("user:/abc/#9223372036854775807/ghi", NULL, 0, 53, 49);

	TEST_VALIDATE_OK ("/abc/#_10/ghi", NULL, 0, 14, 15);
	TEST_VALIDATE_OK ("user:/abc/#_10/ghi", NULL, 0, 19, 15);

	TEST_VALIDATE_OK ("/abc/#1/#_10", NULL, 0, 13, 14);
	TEST_VALIDATE_OK ("user:/abc/#1/#_10", NULL, 0, 18, 14);

	TEST_VALIDATE_OK ("/abc/#____10000/ghi", NULL, 0, 20, 21);
	TEST_VALIDATE_OK ("user:/abc/#____10000/ghi", NULL, 0, 25, 21);

	TEST_VALIDATE_OK ("/abc/#__________________9223372036854775807/ghi", NULL, 0, 48, 49);
	TEST_VALIDATE_OK ("user:/abc/#__________________9223372036854775807/ghi", NULL, 0, 53, 49);

	TEST_VALIDATE_OK ("/abc/\\%/def", NULL, 0, 12, 12);
	TEST_VALIDATE_OK ("user:/abc/\\%/def", NULL, 0, 17, 12);

	TEST_VALIDATE_OK ("/abc/\\./ghi", NULL, 0, 12, 12);
	TEST_VALIDATE_OK ("user:/abc/\\./ghi", NULL, 0, 17, 12);

	TEST_VALIDATE_OK ("/abc/\\../ghi", NULL, 0, 13, 13);
	TEST_VALIDATE_OK ("user:/abc/\\../ghi", NULL, 0, 18, 13);

	TEST_VALIDATE_OK ("/abc/\\#_10/ghi", NULL, 0, 15, 15);
	TEST_VALIDATE_OK ("user:/abc/\\#_10/ghi", NULL, 0, 20, 15);

	TEST_VALIDATE_OK ("/abc/\\#0/ghi", NULL, 0, 13, 13);
	TEST_VALIDATE_OK ("user:/abc/\\#0/ghi", NULL, 0, 18, 13);

	TEST_VALIDATE_OK ("/abc/\\.def/ghi", NULL, 0, 15, 15);
	TEST_VALIDATE_OK ("user:/abc/\\.def/ghi", NULL, 0, 20, 15);

	TEST_VALIDATE_OK ("/abc/\\#10/ghi", NULL, 0, 14, 14);
	TEST_VALIDATE_OK ("user:/abc/\\#10/ghi", NULL, 0, 19, 14);

	TEST_VALIDATE_OK ("/abc/\\#01/ghi", NULL, 0, 14, 14);
	TEST_VALIDATE_OK ("user:/abc/\\#01/ghi", NULL, 0, 19, 14);

	TEST_VALIDATE_OK ("/abc/\\#10000/ghi", NULL, 0, 17, 17);
	TEST_VALIDATE_OK ("user:/abc/\\#10000/ghi", NULL, 0, 22, 17);

	TEST_VALIDATE_OK ("/abc/\\#9223372036854775807/ghi", NULL, 0, 31, 31);
	TEST_VALIDATE_OK ("user:/abc/\\#9223372036854775807/ghi", NULL, 0, 36, 31);

	TEST_VALIDATE_OK ("/abc\\/def/ghi", NULL, 0, 14, 14);
	TEST_VALIDATE_OK ("user:/abc\\/def/ghi", NULL, 0, 19, 14);

	TEST_VALIDATE_OK ("/abc/de\\\\f/ghi", NULL, 0, 15, 15);
	TEST_VALIDATE_OK ("user:/abc/de\\\\f/ghi", NULL, 0, 20, 15);

	TEST_VALIDATE_OK ("/abc/de\\\\\\\\f/ghi", NULL, 0, 17, 16);
	TEST_VALIDATE_OK ("user:/abc/de\\\\\\\\f/ghi", NULL, 0, 22, 16);

	TEST_VALIDATE_OK ("/abc/de\\\\\\\\\\\\f/ghi", NULL, 0, 19, 17);
	TEST_VALIDATE_OK ("user:/abc/de\\\\\\\\\\\\f/ghi", NULL, 0, 24, 17);

	TEST_VALIDATE_OK ("/abc/def/ghi\\/", NULL, 0, 15, 15);
	TEST_VALIDATE_OK ("user:/abc/def/ghi\\/", NULL, 0, 20, 15);

	TEST_VALIDATE_OK ("/abc/\\@/def", NULL, 0, 12, 12);
	TEST_VALIDATE_OK ("user:/abc/\\@/def", NULL, 0, 17, 12);

	TEST_VALIDATE_OK ("/abc/#/def", NULL, 0, 11, 12);
	TEST_VALIDATE_OK ("user:/abc/#/def", NULL, 0, 16, 12);

	TEST_VALIDATE_OK ("/abc/\\#def/ghi", NULL, 0, 15, 15);
	TEST_VALIDATE_OK ("user:/abc/\\#def/ghi", NULL, 0, 20, 15);

	TEST_VALIDATE_OK ("/abc/def/ghi/%", NULL, 0, 15, 15);
	TEST_VALIDATE_OK ("user:/abc/def/ghi/%", NULL, 0, 20, 15);
	TEST_VALIDATE_OK ("user:/abc/..", NULL, 0, 7, 3);

	TEST_VALIDATE_OK ("///////.", NULL, 0, 2, 3);
	TEST_VALIDATE_OK ("/.", NULL, 0, 2, 3);
	TEST_VALIDATE_OK ("///.////.", NULL, 0, 2, 3);
	TEST_VALIDATE_OK ("////.///", NULL, 0, 2, 3);
	TEST_VALIDATE_OK ("//.///.//.", NULL, 0, 2, 3);
	TEST_VALIDATE_OK ("/./a", NULL, 0, 3, 4);

	TEST_VALIDATE_OK ("/\\\\", NULL, 0, 4, 4);
	TEST_VALIDATE_OK ("/\\#0/\\#1", NULL, 0, 9, 8);

	TEST_VALIDATE_OK ("///////.", "/", 3, 2, 3);
	TEST_VALIDATE_OK ("/.", "/", 3, 2, 3);
	TEST_VALIDATE_OK ("///.////.", "/", 3, 2, 3);
	TEST_VALIDATE_OK ("////.///", "/", 3, 2, 3);
	TEST_VALIDATE_OK ("//.///.//.", "/", 3, 2, 3);
	TEST_VALIDATE_OK ("/./a", "/", 3, 3, 4);

	TEST_VALIDATE_OK ("", "/", 3, 2, 3);
	TEST_VALIDATE_OK ("", "user:/", 3, 7, 3);

	TEST_VALIDATE_OK ("", "/abc", 6, 5, 6);
	TEST_VALIDATE_OK ("/", "/abc", 6, 5, 6);
	TEST_VALIDATE_OK ("%", "/abc", 6, 7, 7);

	TEST_VALIDATE_OK ("/abc/def/ghi", "/abc", 6, 17, 18);
	TEST_VALIDATE_OK ("/abc/def/ghi/", "/abc", 6, 17, 18);
	TEST_VALIDATE_OK ("/abc//def////ghi/", "/abc", 6, 17, 18);
	TEST_VALIDATE_OK ("/////////", "/abc", 6, 5, 6);
	TEST_VALIDATE_OK ("/abc/%/def", "/abc", 6, 15, 15);
	TEST_VALIDATE_OK ("/abc/d@ef/ghi", "/abc", 6, 18, 19);
	TEST_VALIDATE_OK ("/abc/.def/ghi", "/abc", 6, 18, 19);

	TEST_VALIDATE_OK ("abc", "/", 3, 5, 6);
	TEST_VALIDATE_OK ("abc", "/abc", 6, 9, 10);
	TEST_VALIDATE_OK (".", "/abc", 6, 5, 6);

	TEST_VALIDATE_OK ("../abc", "/x", 4, 5, 6);
	TEST_VALIDATE_OK ("../../ab/c", "/x/y", 6, 6, 7);

	TEST_VALIDATE_OK ("/..", "/abc", 6, 2, 3);
	TEST_VALIDATE_OK ("..", "/abc", 6, 2, 3);
	TEST_VALIDATE_OK ("/../..", "/abc/def", 10, 2, 3);
	TEST_VALIDATE_OK ("../..", "/abc/def", 10, 2, 3);
	TEST_VALIDATE_OK ("/def/../..", "/abc", 6, 2, 3);
	TEST_VALIDATE_OK ("def/../..", "/abc", 6, 2, 3);
	TEST_VALIDATE_OK ("/%/../..", "/abc", 6, 2, 3);
	TEST_VALIDATE_OK ("%/../..", "/abc", 6, 2, 3);

	TEST_VALIDATE_OK ("/..///////////////..", "/abc/def", 10, 2, 3);

	TEST_VALIDATE_OK ("..", "/%", 4, 2, 3);
	TEST_VALIDATE_OK ("..", "/\\/", 5, 2, 3);
	TEST_VALIDATE_OK ("/\\//..", "/abc", 6, 5, 6);
	TEST_VALIDATE_OK ("/\\./..", "/abc", 6, 5, 6);
	TEST_VALIDATE_OK ("/a/./..", "/abc", 6, 7, 8);
	TEST_VALIDATE_OK ("/\\\\\\\\\\\\/..", "/abc", 6, 5, 6);

	TEST_VALIDATE_OK ("../.", "/%", 4, 2, 3);

	TEST_VALIDATE_OK ("/#0/..", "/", 3, 2, 3);
	TEST_VALIDATE_OK ("/#10/..", "/", 3, 2, 3);
	TEST_VALIDATE_OK ("/#_10/..", "/", 3, 2, 3);
	TEST_VALIDATE_OK ("/\\#0/..", "/", 3, 2, 3);

	TEST_VALIDATE_OK ("user:", "/", 3, 7, 8);
	TEST_VALIDATE_OK ("user", "/", 3, 6, 7);

	TEST_VALIDATE_OK ("abc", "user:/", 3, 10, 6);
	TEST_VALIDATE_OK ("abc", "user:/abc", 6, 14, 10);
	TEST_VALIDATE_OK ("..", "user:/abc", 6, 7, 3);

	TEST_VALIDATE_OK ("/./../..", "/abc", 6, 2, 3);
	TEST_VALIDATE_OK ("./../..", "/abc", 6, 2, 3);
	TEST_VALIDATE_OK ("abc/..", "user:/", 3, 7, 3);

	TEST_VALIDATE_OK ("/abc/%def/ghi", NULL, 0, 14, 15);
	TEST_VALIDATE_OK ("user:/abc/%def/ghi", NULL, 0, 19, 15);

	succeed_if (!elektraKeyNameValidate (NULL, NULL, NULL, NULL), "(NULL) SHOULD NOT BE a valid key name");

	TEST_VALIDATE_ERROR ("", NULL, 0);
	TEST_VALIDATE_ERROR ("user/", NULL, 0);
	TEST_VALIDATE_ERROR ("user:", NULL, 0);
	TEST_VALIDATE_ERROR ("user", NULL, 0);

	TEST_VALIDATE_ERROR ("/\\", NULL, 0);
	TEST_VALIDATE_ERROR ("/\\\\\\", NULL, 0);

	TEST_VALIDATE_ERROR ("user:abc:/", NULL, 0);
	TEST_VALIDATE_ERROR ("abc:/", NULL, 0);
	TEST_VALIDATE_ERROR ("abc", NULL, 0);

	TEST_VALIDATE_ERROR ("/..", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/..", NULL, 0);
	TEST_VALIDATE_ERROR ("/abc/../..", NULL, 0);

	TEST_VALIDATE_ERROR ("/../..", "/abc", 6);
	TEST_VALIDATE_ERROR ("@", "/abc", 6);

	TEST_VALIDATE_ERROR ("/abc/#92233720368547758071/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/#92233720368547758071/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/#9223372036854775808/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/#9223372036854775808/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/#__________________9223372036854775808/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/#__________________9223372036854775808/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/#01/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/#01/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/#__10/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/#__10/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/#_100/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/#_100/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/#___10/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/#___10/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/\\def/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/\\def/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/d\\.ef/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/d\\.ef/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/d\\#ef/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/d\\#ef/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/d\\%ef/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/d\\%ef/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/d\\@ef/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/d\\@ef/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/@/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/@/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/@def/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/@def/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/abc/\\def/ghi", NULL, 0);
	TEST_VALIDATE_ERROR ("user:/abc/\\def/ghi", NULL, 0);

	TEST_VALIDATE_ERROR ("/./../../..", "/abc", 6);
	TEST_VALIDATE_ERROR ("./../../..", "/abc", 6);

	TEST_VALIDATE_ERROR ("..", "/", 3);
	TEST_VALIDATE_ERROR ("../..", "/a\\/b", 6);
	TEST_VALIDATE_ERROR ("..", "user:/", 3);

	TEST_VALIDATE_ERROR ("../user", "user:/", 3);
	TEST_VALIDATE_ERROR ("../../../../../../..//user", "/much/more/level/1/2/3", 23);
	TEST_VALIDATE_ERROR ("..///../../../../../../..//user", "/much/more/level/1/2/3", 23);
	TEST_VALIDATE_ERROR ("..///../../..////../../../..//user", "/much/more/level/1/2/3", 23);
	TEST_VALIDATE_ERROR ("../../....///../../..////../../../..//user", "/much/more/level/1/2/3", 23);

	TEST_VALIDATE_ERROR ("/\\\\\\%", NULL, 0);
	TEST_VALIDATE_ERROR ("/\\\\\\\\\\%", NULL, 0);
}

#undef TEST_VALIDATE_OK
#undef TEST_VALIDATE_ERROR

#define TEST_CANONICALIZE_OK(name, prefix, cname)                                                                                          \
	do                                                                                                                                 \
	{                                                                                                                                  \
		size_t bufLen = sizeof (prefix) > sizeof (cname) ? sizeof (prefix) : sizeof (cname);                                       \
		char * buf = elektraMalloc (bufLen);                                                                                       \
		memset (buf, ' ', bufLen - 1);                                                                                             \
		strcpy (buf, prefix);                                                                                                      \
		elektraKeyNameCanonicalize (name, &buf, sizeof (cname), sizeof (prefix) - 1);                                              \
		succeed_if_same_string (buf, cname);                                                                                       \
		elektraFree (buf);                                                                                                         \
	} while (0)

static void test_canonicalize (void)
{
	TEST_CANONICALIZE_OK ("/", "", "/");
	TEST_CANONICALIZE_OK ("proc:/", "", "proc:/");
	TEST_CANONICALIZE_OK ("dir:/", "", "dir:/");
	TEST_CANONICALIZE_OK ("user:/", "", "user:/");
	TEST_CANONICALIZE_OK ("system:/", "", "system:/");
	TEST_CANONICALIZE_OK ("spec:/", "", "spec:/");
	TEST_CANONICALIZE_OK ("meta:/", "", "meta:/");
	TEST_CANONICALIZE_OK ("default:/", "", "default:/");

	TEST_CANONICALIZE_OK ("/abc/def/ghi", "", "/abc/def/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/def/ghi", "", "user:/abc/def/ghi");

	TEST_CANONICALIZE_OK ("/abc/def/ghi/", "", "/abc/def/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/def/ghi/", "", "user:/abc/def/ghi");

	TEST_CANONICALIZE_OK ("/abc//def////ghi/", "", "/abc/def/ghi");
	TEST_CANONICALIZE_OK ("user://abc////def/ghi/", "", "user:/abc/def/ghi");

	TEST_CANONICALIZE_OK ("/////////", "", "/");
	TEST_CANONICALIZE_OK ("user://///////", "", "user:/");

	TEST_CANONICALIZE_OK ("/abc/%/def", "", "/abc/%/def");
	TEST_CANONICALIZE_OK ("user:/abc/%/def", "", "user:/abc/%/def");

	TEST_CANONICALIZE_OK ("/abc/d@ef/ghi", "", "/abc/d@ef/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/d@ef/ghi", "", "user:/abc/d@ef/ghi");

	TEST_CANONICALIZE_OK ("/abc/.def/ghi", "", "/abc/.def/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/.def/ghi", "", "user:/abc/.def/ghi");

	TEST_CANONICALIZE_OK ("/abc/./ghi", "", "/abc/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/./ghi", "", "user:/abc/ghi");

	TEST_CANONICALIZE_OK ("/abc/../ghi", "", "/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/../ghi", "", "user:/ghi");

	TEST_CANONICALIZE_OK ("/abc/.../ghi", "", "/abc/.../ghi");
	TEST_CANONICALIZE_OK ("user:/abc/.../ghi", "", "user:/abc/.../ghi");

	TEST_CANONICALIZE_OK ("/abc/..../ghi", "", "/abc/..../ghi");
	TEST_CANONICALIZE_OK ("user:/abc/..../ghi", "", "user:/abc/..../ghi");

	TEST_CANONICALIZE_OK ("/abc/#0/ghi", "", "/abc/#0/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/#0/ghi", "", "user:/abc/#0/ghi");

	TEST_CANONICALIZE_OK ("/abc/#1/ghi", "", "/abc/#1/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/#1/ghi", "", "user:/abc/#1/ghi");

	TEST_CANONICALIZE_OK ("/abc/#5/ghi", "", "/abc/#5/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/#5/ghi", "", "user:/abc/#5/ghi");

	TEST_CANONICALIZE_OK ("/abc/#10/ghi", "", "/abc/#_10/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/#10/ghi", "", "user:/abc/#_10/ghi");

	TEST_CANONICALIZE_OK ("/abc/#10000/ghi", "", "/abc/#____10000/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/#10000/ghi", "", "user:/abc/#____10000/ghi");

	TEST_CANONICALIZE_OK ("/abc/#9223372036854775807/ghi", "", "/abc/#__________________9223372036854775807/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/#9223372036854775807/ghi", "", "user:/abc/#__________________9223372036854775807/ghi");

	TEST_CANONICALIZE_OK ("/abc/#_10/ghi", "", "/abc/#_10/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/#_10/ghi", "", "user:/abc/#_10/ghi");

	TEST_CANONICALIZE_OK ("/abc/#____10000/ghi", "", "/abc/#____10000/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/#____10000/ghi", "", "user:/abc/#____10000/ghi");

	TEST_CANONICALIZE_OK ("/abc/#__________________9223372036854775807/ghi", "", "/abc/#__________________9223372036854775807/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/#__________________9223372036854775807/ghi", "",
			      "user:/abc/#__________________9223372036854775807/ghi");

	TEST_CANONICALIZE_OK ("/abc/#10/ghi", "", "/abc/#_10/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/#10/ghi", "", "user:/abc/#_10/ghi");

	TEST_CANONICALIZE_OK ("/abc/#1/#_10", "", "/abc/#1/#_10");
	TEST_CANONICALIZE_OK ("user:/abc/#1/#_10", "", "user:/abc/#1/#_10");

	TEST_CANONICALIZE_OK ("/abc/\\%/def", "", "/abc/\\%/def");
	TEST_CANONICALIZE_OK ("user:/abc/\\%/def", "", "user:/abc/\\%/def");

	TEST_CANONICALIZE_OK ("/abc/\\./ghi", "", "/abc/\\./ghi");
	TEST_CANONICALIZE_OK ("user:/abc/\\./ghi", "", "user:/abc/\\./ghi");

	TEST_CANONICALIZE_OK ("/abc/\\../ghi", "", "/abc/\\../ghi");
	TEST_CANONICALIZE_OK ("user:/abc/\\../ghi", "", "user:/abc/\\../ghi");

	TEST_CANONICALIZE_OK ("/abc/\\#10/ghi", "", "/abc/\\#10/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/\\#10/ghi", "", "user:/abc/\\#10/ghi");

	TEST_CANONICALIZE_OK ("/abc/\\#10000/ghi", "", "/abc/\\#10000/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/\\#10000/ghi", "", "user:/abc/\\#10000/ghi");

	TEST_CANONICALIZE_OK ("/abc/\\#9223372036854775807/ghi", "", "/abc/\\#9223372036854775807/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/\\#9223372036854775807/ghi", "", "user:/abc/\\#9223372036854775807/ghi");

	TEST_CANONICALIZE_OK ("/abc\\/def/ghi", "", "/abc\\/def/ghi");
	TEST_CANONICALIZE_OK ("user:/abc\\/def/ghi", "", "user:/abc\\/def/ghi");

	TEST_CANONICALIZE_OK ("/abc/de\\\\f/ghi", "", "/abc/de\\\\f/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/de\\\\f/ghi", "", "user:/abc/de\\\\f/ghi");

	TEST_CANONICALIZE_OK ("/abc/def/ghi\\/", "", "/abc/def/ghi\\/");
	TEST_CANONICALIZE_OK ("user:/abc/def/ghi\\/", "", "user:/abc/def/ghi\\/");

	TEST_CANONICALIZE_OK ("/abc/\\@/def", "", "/abc/\\@/def");
	TEST_CANONICALIZE_OK ("user:/abc/\\@/def", "", "user:/abc/\\@/def");

	TEST_CANONICALIZE_OK ("/abc/#/def", "", "/abc/#/def");
	TEST_CANONICALIZE_OK ("user:/abc/#/def", "", "user:/abc/#/def");

	TEST_CANONICALIZE_OK ("/abc/\\#123/ghi", "", "/abc/\\#123/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/\\#123/ghi", "", "user:/abc/\\#123/ghi");

	TEST_CANONICALIZE_OK ("/abc/\\#def/ghi", "", "/abc/\\#def/ghi");
	TEST_CANONICALIZE_OK ("user:/abc/\\#def/ghi", "", "user:/abc/\\#def/ghi");

	TEST_CANONICALIZE_OK ("", "/abc", "/abc");
	TEST_CANONICALIZE_OK ("/", "/abc", "/abc");
	TEST_CANONICALIZE_OK ("%", "/abc", "/abc/%");

	TEST_CANONICALIZE_OK ("", "/", "/");
	TEST_CANONICALIZE_OK ("/", "/", "/");
	TEST_CANONICALIZE_OK ("%", "/", "/%");

	TEST_CANONICALIZE_OK ("", "user:/", "user:/");
	TEST_CANONICALIZE_OK ("/", "user:/", "user:/");
	TEST_CANONICALIZE_OK ("%", "user:/", "user:/%");

	TEST_CANONICALIZE_OK ("/abc/def/ghi", "/abc", "/abc/abc/def/ghi");
	TEST_CANONICALIZE_OK ("/abc/def/ghi/", "/abc", "/abc/abc/def/ghi");
	TEST_CANONICALIZE_OK ("/abc//def////ghi/", "/abc", "/abc/abc/def/ghi");
	TEST_CANONICALIZE_OK ("/////////", "/abc", "/abc");
	TEST_CANONICALIZE_OK ("/abc/%/def", "/abc", "/abc/abc/%/def");
	TEST_CANONICALIZE_OK ("/abc/d@ef/ghi", "/abc", "/abc/abc/d@ef/ghi");
	TEST_CANONICALIZE_OK ("/abc/.def/ghi", "/abc", "/abc/abc/.def/ghi");

	TEST_CANONICALIZE_OK ("abc", "/", "/abc");
	TEST_CANONICALIZE_OK ("abc", "/abc", "/abc/abc");
	TEST_CANONICALIZE_OK ("..", "/abc", "/");
	TEST_CANONICALIZE_OK ("../", "/abc", "/");
	TEST_CANONICALIZE_OK ("/../..", "/abc/def", "/");
	TEST_CANONICALIZE_OK ("../..", "/abc/def", "/");

	TEST_CANONICALIZE_OK ("user/", "/", "/user");
	TEST_CANONICALIZE_OK ("user:", "/", "/user:");
	TEST_CANONICALIZE_OK ("user", "/", "/user");

	TEST_CANONICALIZE_OK ("..", "user:/abc", "user:/");
	TEST_CANONICALIZE_OK ("abc/..", "user:/", "user:/");
	TEST_CANONICALIZE_OK ("user:/abc/..", "", "user:/");

	TEST_CANONICALIZE_OK ("/%", "", "/%");
	TEST_CANONICALIZE_OK ("/\\%", "", "/\\%");
	TEST_CANONICALIZE_OK ("/\\\\%", "", "/\\\\%");
	TEST_CANONICALIZE_OK ("/\\\\\\\\%", "", "/\\\\\\\\%");

	TEST_CANONICALIZE_OK ("/\\/", "", "/\\/");
	TEST_CANONICALIZE_OK ("/\\\\", "", "/\\\\");
	TEST_CANONICALIZE_OK ("/\\\\\\/", "", "/\\\\\\/");
	TEST_CANONICALIZE_OK ("/\\\\\\\\", "", "/\\\\\\\\");
	TEST_CANONICALIZE_OK ("/\\\\\\\\\\/", "", "/\\\\\\\\\\/");

	TEST_CANONICALIZE_OK ("/\\//", "", "/\\/");
	TEST_CANONICALIZE_OK ("/\\\\/", "", "/\\\\");
	TEST_CANONICALIZE_OK ("/\\\\\\//", "", "/\\\\\\/");
	TEST_CANONICALIZE_OK ("/\\\\\\\\/", "", "/\\\\\\\\");
	TEST_CANONICALIZE_OK ("/\\\\\\\\\\//", "", "/\\\\\\\\\\/");

	TEST_CANONICALIZE_OK ("user:/\\/", "", "user:/\\/");
	TEST_CANONICALIZE_OK ("user:/\\\\", "", "user:/\\\\");
	TEST_CANONICALIZE_OK ("user:/\\\\\\/", "", "user:/\\\\\\/");
	TEST_CANONICALIZE_OK ("user:/\\\\\\\\", "", "user:/\\\\\\\\");
	TEST_CANONICALIZE_OK ("user:/\\\\\\\\\\/", "", "user:/\\\\\\\\\\/");

	TEST_CANONICALIZE_OK ("user:/\\//", "", "user:/\\/");
	TEST_CANONICALIZE_OK ("user:/\\\\/", "", "user:/\\\\");
	TEST_CANONICALIZE_OK ("user:/\\\\\\//", "", "user:/\\\\\\/");
	TEST_CANONICALIZE_OK ("user:/\\\\\\\\/", "", "user:/\\\\\\\\");
	TEST_CANONICALIZE_OK ("user:/\\\\\\\\\\//", "", "user:/\\\\\\\\\\/");

	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\/", "", "user:/tests/plugin/\\/");
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\", "", "user:/tests/plugin/\\\\");
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\/", "", "user:/tests/plugin/\\\\\\/");
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\\\", "", "user:/tests/plugin/\\\\\\\\");
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\\\\\/", "", "user:/tests/plugin/\\\\\\\\\\/");

	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\//", "", "user:/tests/plugin/\\/");
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\/", "", "user:/tests/plugin/\\\\");
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\//", "", "user:/tests/plugin/\\\\\\/");
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\\\/", "", "user:/tests/plugin/\\\\\\\\");
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\\\\\//", "", "user:/tests/plugin/\\\\\\\\\\/");

	TEST_CANONICALIZE_OK ("/abc/%def/ghi", "", "/abc/%def/ghi");
	TEST_CANONICALIZE_OK ("/abc/%d%ef%/ghi", "", "/abc/%d%ef%/ghi");
	TEST_CANONICALIZE_OK ("/abc/%def%/ghi", "", "/abc/%def%/ghi");
	TEST_CANONICALIZE_OK ("/abc/d%ef/ghi", "", "/abc/d%ef/ghi");
}

#undef TEST_CANONICALIZE_OK

static const char * keyNsNames[] = { "KEY_NS_NONE", "KEY_NS_CASCADING", "KEY_NS_META",	 "KEY_NS_SPEC",	  "KEY_NS_PROC",
				     "KEY_NS_DIR",  "KEY_NS_USER",	"KEY_NS_SYSTEM", "KEY_NS_DEFAULT" };

#define succeed_if_same_uname(name, pu1, pu2, size2)                                                                                       \
	do                                                                                                                                 \
	{                                                                                                                                  \
		const char * u1 = pu1;                                                                                                     \
		const char * base2 = pu2;                                                                                                  \
		const char * u2 = base2;                                                                                                   \
		size_t i = 1;                                                                                                              \
		int success = 1;                                                                                                           \
		succeed_if_fmt (success = (success && u1[0] == u2[0]), "\tnamespaces not equal: %s vs %s", keyNsNames[(int) u1[0]],        \
				keyNsNames[(int) u2[0]]);                                                                                  \
		++u1;                                                                                                                      \
		++u2;                                                                                                                      \
		while (u2 < base2 + size2)                                                                                                 \
		{                                                                                                                          \
			succeed_if_fmt (success = (success && strcmp (u1, u2) == 0), "\tpart no %zu \"%s\" is not equal to \"%s\"", i, u1, \
					u2) u2 += strlen (u2) + 1;                                                                         \
			u1 += strlen (u1) + 1;                                                                                             \
			++i;                                                                                                               \
		}                                                                                                                          \
		if (!success) yield_error_fmt ("uname for '%s' didn't match", name);                                                       \
	} while (0)

#define TEST_UNESCAPE_OK(name, ns, uname)                                                                                                  \
	do                                                                                                                                 \
	{                                                                                                                                  \
		char buffer[sizeof (uname) + 2];                                                                                           \
		char * buf = buffer;                                                                                                       \
		elektraKeyNameUnescape (name, &buf);                                                                                       \
		char expected[sizeof (uname) + 2];                                                                                         \
		expected[0] = ns;                                                                                                          \
		memcpy (expected + 1, uname, sizeof (uname) - 1);                                                                          \
		expected[sizeof (uname)] = '\0';                                                                                           \
		succeed_if_same_uname (name, buffer, expected, sizeof (uname) + 1);                                                        \
	} while (0)

static void test_unescape (void)
{
	TEST_UNESCAPE_OK ("/", KEY_NS_CASCADING, "\0");
	TEST_UNESCAPE_OK ("proc:/", KEY_NS_PROC, "\0");
	TEST_UNESCAPE_OK ("dir:/", KEY_NS_DIR, "\0");
	TEST_UNESCAPE_OK ("user:/", KEY_NS_USER, "\0");
	TEST_UNESCAPE_OK ("system:/", KEY_NS_SYSTEM, "\0");
	TEST_UNESCAPE_OK ("spec:/", KEY_NS_SPEC, "\0");
	TEST_UNESCAPE_OK ("meta:/", KEY_NS_META, "\0");
	TEST_UNESCAPE_OK ("default:/", KEY_NS_DEFAULT, "\0");

	TEST_UNESCAPE_OK ("/abc/def/ghi", KEY_NS_CASCADING, "\0abc\0def\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/def/ghi", KEY_NS_USER, "\0abc\0def\0ghi");

	TEST_UNESCAPE_OK ("/abc/def/ghi/", KEY_NS_CASCADING, "\0abc\0def\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/def/ghi/", KEY_NS_USER, "\0abc\0def\0ghi");

	TEST_UNESCAPE_OK ("/abc/%/def", KEY_NS_CASCADING, "\0abc\0\0def");
	TEST_UNESCAPE_OK ("user:/abc/%/def", KEY_NS_USER, "\0abc\0\0def");

	TEST_UNESCAPE_OK ("/abc/d@ef/ghi", KEY_NS_CASCADING, "\0abc\0d@ef\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/d@ef/ghi", KEY_NS_USER, "\0abc\0d@ef\0ghi");

	TEST_UNESCAPE_OK ("/abc/.def/ghi", KEY_NS_CASCADING, "\0abc\0.def\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/.def/ghi", KEY_NS_USER, "\0abc\0.def\0ghi");

	TEST_UNESCAPE_OK ("/abc/.../ghi", KEY_NS_CASCADING, "\0abc\0...\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/.../ghi", KEY_NS_USER, "\0abc\0...\0ghi");

	TEST_UNESCAPE_OK ("/abc/..../ghi", KEY_NS_CASCADING, "\0abc\0....\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/..../ghi", KEY_NS_USER, "\0abc\0....\0ghi");

	TEST_UNESCAPE_OK ("/abc/#0/ghi", KEY_NS_CASCADING, "\0abc\0#0\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/#0/ghi", KEY_NS_USER, "\0abc\0#0\0ghi");

	TEST_UNESCAPE_OK ("/abc/#1/ghi", KEY_NS_CASCADING, "\0abc\0#1\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/#1/ghi", KEY_NS_USER, "\0abc\0#1\0ghi");

	TEST_UNESCAPE_OK ("/abc/#5/ghi", KEY_NS_CASCADING, "\0abc\0#5\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/#5/ghi", KEY_NS_USER, "\0abc\0#5\0ghi");

	TEST_UNESCAPE_OK ("/abc/#_10/ghi", KEY_NS_CASCADING, "\0abc\0#_10\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/#_10/ghi", KEY_NS_USER, "\0abc\0#_10\0ghi");

	TEST_UNESCAPE_OK ("/abc/#____10000/ghi", KEY_NS_CASCADING, "\0abc\0#____10000\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/#____10000/ghi", KEY_NS_USER, "\0abc\0#____10000\0ghi");

	TEST_UNESCAPE_OK ("/abc/#__________________9223372036854775807/ghi", KEY_NS_CASCADING,
			  "\0abc\0#__________________9223372036854775807\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/#__________________9223372036854775807/ghi", KEY_NS_USER,
			  "\0abc\0#__________________9223372036854775807\0ghi");

	TEST_UNESCAPE_OK ("/abc/\\%/def", KEY_NS_CASCADING, "\0abc\0%\0def");
	TEST_UNESCAPE_OK ("user:/abc/\\%/def", KEY_NS_USER, "\0abc\0%\0def");

	TEST_UNESCAPE_OK ("/abc/\\./ghi", KEY_NS_CASCADING, "\0abc\0.\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/\\./ghi", KEY_NS_USER, "\0abc\0.\0ghi");

	TEST_UNESCAPE_OK ("/abc/\\../ghi", KEY_NS_CASCADING, "\0abc\0..\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/\\../ghi", KEY_NS_USER, "\0abc\0..\0ghi");

	TEST_UNESCAPE_OK ("/abc\\/def/ghi", KEY_NS_CASCADING, "\0abc/def\0ghi");
	TEST_UNESCAPE_OK ("user:/abc\\/def/ghi", KEY_NS_USER, "\0abc/def\0ghi");

	TEST_UNESCAPE_OK ("/abc/de\\\\f/ghi", KEY_NS_CASCADING, "\0abc\0de\\f\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/de\\\\f/ghi", KEY_NS_USER, "\0abc\0de\\f\0ghi");

	TEST_UNESCAPE_OK ("/abc/def/ghi\\/", KEY_NS_CASCADING, "\0abc\0def\0ghi/");
	TEST_UNESCAPE_OK ("user:/abc/def/ghi\\/", KEY_NS_USER, "\0abc\0def\0ghi/");

	TEST_UNESCAPE_OK ("/abc/\\@/def", KEY_NS_CASCADING, "\0abc\0@\0def");
	TEST_UNESCAPE_OK ("user:/abc/\\@/def", KEY_NS_USER, "\0abc\0@\0def");

	TEST_UNESCAPE_OK ("/abc/#/def", KEY_NS_CASCADING, "\0abc\0#\0def");
	TEST_UNESCAPE_OK ("user:/abc/#/def", KEY_NS_USER, "\0abc\0#\0def");

	TEST_UNESCAPE_OK ("/abc/\\#def/ghi", KEY_NS_CASCADING, "\0abc\0#def\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/\\#def/ghi", KEY_NS_USER, "\0abc\0#def\0ghi");

	TEST_UNESCAPE_OK ("/abc/\\#123/ghi", KEY_NS_CASCADING, "\0abc\0#123\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/\\#123/ghi", KEY_NS_USER, "\0abc\0#123\0ghi");

	TEST_UNESCAPE_OK ("/abc/\\#123/ghi", KEY_NS_CASCADING, "\0abc\0#123\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/\\#123/ghi", KEY_NS_USER, "\0abc\0#123\0ghi");

	TEST_UNESCAPE_OK ("/abc/\\#__10/ghi", KEY_NS_CASCADING, "\0abc\0#__10\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/\\#_100/ghi", KEY_NS_USER, "\0abc\0#_100\0ghi");

	TEST_UNESCAPE_OK ("/abc/#1/#_10", KEY_NS_CASCADING, "\0abc\0#1\0#_10");
	TEST_UNESCAPE_OK ("user:/abc/#1/#_10", KEY_NS_USER, "\0abc\0#1\0#_10");

	TEST_UNESCAPE_OK ("/%", KEY_NS_CASCADING, "\0");
	TEST_UNESCAPE_OK ("/\\%", KEY_NS_CASCADING, "\0%");
	TEST_UNESCAPE_OK ("/\\\\%", KEY_NS_CASCADING, "\0\\%");
	TEST_UNESCAPE_OK ("/\\\\\\\\%", KEY_NS_CASCADING, "\0\\\\%");

	TEST_UNESCAPE_OK ("/\\/", KEY_NS_CASCADING, "\0/");
	TEST_UNESCAPE_OK ("/\\\\", KEY_NS_CASCADING, "\0\\");
	TEST_UNESCAPE_OK ("/\\\\\\/", KEY_NS_CASCADING, "\0\\/");
	TEST_UNESCAPE_OK ("/\\\\\\\\", KEY_NS_CASCADING, "\0\\\\");
	TEST_UNESCAPE_OK ("/\\\\\\\\\\/", KEY_NS_CASCADING, "\0\\\\/");

	TEST_UNESCAPE_OK ("user:/\\/", KEY_NS_USER, "\0/");
	TEST_UNESCAPE_OK ("user:/\\\\", KEY_NS_USER, "\0\\");
	TEST_UNESCAPE_OK ("user:/\\\\\\/", KEY_NS_USER, "\0\\/");
	TEST_UNESCAPE_OK ("user:/\\\\\\\\", KEY_NS_USER, "\0\\\\");
	TEST_UNESCAPE_OK ("user:/\\\\\\\\\\/", KEY_NS_USER, "\0\\\\/");

	TEST_UNESCAPE_OK ("user:/tests/plugin/\\/", KEY_NS_USER, "\0tests\0plugin\0/");
	TEST_UNESCAPE_OK ("user:/tests/plugin/\\\\", KEY_NS_USER, "\0tests\0plugin\0\\");
	TEST_UNESCAPE_OK ("user:/tests/plugin/\\\\\\/", KEY_NS_USER, "\0tests\0plugin\0\\/");
	TEST_UNESCAPE_OK ("user:/tests/plugin/\\\\\\\\", KEY_NS_USER, "\0tests\0plugin\0\\\\");
	TEST_UNESCAPE_OK ("user:/tests/plugin/\\\\\\\\\\/", KEY_NS_USER, "\0tests\0plugin\0\\\\/");

	TEST_UNESCAPE_OK ("/abc/%def/ghi", KEY_NS_CASCADING, "\0abc\0%def\0ghi");
	TEST_UNESCAPE_OK ("/abc/%d%ef%/ghi", KEY_NS_CASCADING, "\0abc\0%d%ef%\0ghi");
	TEST_UNESCAPE_OK ("/abc/%def%/ghi", KEY_NS_CASCADING, "\0abc\0%def%\0ghi");
	TEST_UNESCAPE_OK ("/abc/d%ef/ghi", KEY_NS_CASCADING, "\0abc\0d%ef\0ghi");
}

#undef TEST_UNESCAPE_OK

#define TEST_ESCAPE_PART_OK(upart, part)                                                                                                   \
	do                                                                                                                                 \
	{                                                                                                                                  \
		char * escaped = NULL;                                                                                                     \
		elektraKeyNameEscapePart (upart, &escaped);                                                                                \
		succeed_if_same_string (part, escaped);                                                                                    \
		elektraFree (escaped);                                                                                                     \
	} while (0)

static void test_escapePart (void)
{
	TEST_ESCAPE_PART_OK ("abc", "abc");
	TEST_ESCAPE_PART_OK ("", "%");
	TEST_ESCAPE_PART_OK ("d@ef", "d@ef");
	TEST_ESCAPE_PART_OK (".def", ".def");
	TEST_ESCAPE_PART_OK ("...", "...");
	TEST_ESCAPE_PART_OK ("....", "....");
	TEST_ESCAPE_PART_OK ("#0", "#0");
	TEST_ESCAPE_PART_OK ("#1", "#1");
	TEST_ESCAPE_PART_OK ("#5", "#5");
	TEST_ESCAPE_PART_OK ("#_10", "#_10");
	TEST_ESCAPE_PART_OK ("#____10000", "#____10000");
	TEST_ESCAPE_PART_OK ("#__________________9223372036854775807", "#__________________9223372036854775807");
	TEST_ESCAPE_PART_OK ("%", "\\%");
	TEST_ESCAPE_PART_OK (".", "\\.");
	TEST_ESCAPE_PART_OK ("..", "\\..");
	TEST_ESCAPE_PART_OK ("/def", "\\/def");
	TEST_ESCAPE_PART_OK ("de\\f", "de\\\\f");
	TEST_ESCAPE_PART_OK ("def/", "def\\/");
	TEST_ESCAPE_PART_OK ("@", "\\@");
	TEST_ESCAPE_PART_OK ("#", "#");
	TEST_ESCAPE_PART_OK ("#def", "\\#def");
	TEST_ESCAPE_PART_OK ("#123", "\\#123");
	TEST_ESCAPE_PART_OK ("#__10", "\\#__10");
	TEST_ESCAPE_PART_OK ("#_100", "\\#_100");
	TEST_ESCAPE_PART_OK ("#01", "\\#01");
	TEST_ESCAPE_PART_OK ("#9223372036854775808", "\\#9223372036854775808");
	TEST_ESCAPE_PART_OK ("#92233720368547758071", "\\#92233720368547758071");
	TEST_ESCAPE_PART_OK ("d/f", "d\\/f");
	TEST_ESCAPE_PART_OK ("user:", "user:");
	TEST_ESCAPE_PART_OK ("d..f", "d..f");
	TEST_ESCAPE_PART_OK ("d.f", "d.f");
	TEST_ESCAPE_PART_OK ("d\\a", "d\\\\a");
}

#undef TEST_ESCAPE_PART_OK

int main (int argc, char ** argv)
{
	printf (" KEYNAME   TESTS\n");
	printf ("==================\n\n");

	init (argc, argv);

	test_validate ();
	test_canonicalize ();
	test_unescape ();
	test_escapePart ();

	print_result ("test_keyname");

	return nbError;
}

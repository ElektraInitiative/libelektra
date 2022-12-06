/**
 * @file
 *
 * @brief Test suite for functions used in creating key name data.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <elektra/kdbprivate.h>

#include "tests.h"

#define TEST_VALIDATE_OK(name, prefix)                                                                                                     \
	do                                                                                                                                 \
	{                                                                                                                                  \
		const char * p = prefix;                                                                                                   \
		succeed_if_fmt (elektraKeyNameValidate (name, p == NULL), "'%s' + '%s' SHOULD BE a valid key name", p == NULL ? "" : p,    \
				name);                                                                                                     \
	} while (0)

#define TEST_VALIDATE_ERROR(name, prefix)                                                                                                  \
	do                                                                                                                                 \
	{                                                                                                                                  \
		const char * p = prefix;                                                                                                   \
		succeed_if_fmt (!elektraKeyNameValidate (name, p == NULL), "'%s' + '%s' SHOULD NOT BE a valid key name",                   \
				p == NULL ? "" : p, name);                                                                                 \
	} while (0)

static void test_validate (void)
{
	TEST_VALIDATE_OK ("/", NULL);
	TEST_VALIDATE_OK ("proc:/", NULL);
	TEST_VALIDATE_OK ("dir:/", NULL);
	TEST_VALIDATE_OK ("user:/", NULL);
	TEST_VALIDATE_OK ("system:/", NULL);
	TEST_VALIDATE_OK ("spec:/", NULL);
	TEST_VALIDATE_OK ("meta:/", NULL);
	TEST_VALIDATE_OK ("default:/", NULL);

	TEST_VALIDATE_OK ("/a", NULL);
	TEST_VALIDATE_OK ("/ab", NULL);
	TEST_VALIDATE_OK ("/abc", NULL);

	TEST_VALIDATE_OK ("/a", NULL);
	TEST_VALIDATE_OK ("/ab", NULL);
	TEST_VALIDATE_OK ("/abc", NULL);

	TEST_VALIDATE_OK ("/\\%", NULL);
	TEST_VALIDATE_OK ("/\\\\%", NULL);
	TEST_VALIDATE_OK ("/\\\\\\\\%", NULL);

	TEST_VALIDATE_OK ("/\\/", NULL);
	TEST_VALIDATE_OK ("/\\\\/", NULL);
	TEST_VALIDATE_OK ("/\\\\\\/", NULL);
	TEST_VALIDATE_OK ("/\\\\\\\\/", NULL);
	TEST_VALIDATE_OK ("/\\\\\\\\\\/", NULL);

	TEST_VALIDATE_OK ("/\\/", NULL);
	TEST_VALIDATE_OK ("/\\\\", NULL);
	TEST_VALIDATE_OK ("/\\\\\\/", NULL);
	TEST_VALIDATE_OK ("/\\\\\\\\", NULL);
	TEST_VALIDATE_OK ("/\\\\\\\\\\/", NULL);

	TEST_VALIDATE_OK ("/\\//", NULL);
	TEST_VALIDATE_OK ("/\\\\/", NULL);
	TEST_VALIDATE_OK ("/\\\\\\//", NULL);
	TEST_VALIDATE_OK ("/\\\\\\\\/", NULL);
	TEST_VALIDATE_OK ("/\\\\\\\\\\//", NULL);

	TEST_VALIDATE_OK ("user:/\\/", NULL);
	TEST_VALIDATE_OK ("user:/\\\\", NULL);
	TEST_VALIDATE_OK ("user:/\\\\\\/", NULL);
	TEST_VALIDATE_OK ("user:/\\\\\\\\", NULL);
	TEST_VALIDATE_OK ("user:/\\\\\\\\\\/", NULL);

	TEST_VALIDATE_OK ("user:/\\//", NULL);
	TEST_VALIDATE_OK ("user:/\\\\/", NULL);
	TEST_VALIDATE_OK ("user:/\\\\\\//", NULL);
	TEST_VALIDATE_OK ("user:/\\\\\\\\/", NULL);
	TEST_VALIDATE_OK ("user:/\\\\\\\\\\//", NULL);

	TEST_VALIDATE_OK ("user:/tests/plugin/\\/", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\/", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\\\/", NULL);

	TEST_VALIDATE_OK ("user:/tests/plugin/\\//", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\/", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\//", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\/", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\\\//", NULL);

	TEST_VALIDATE_OK ("/\\//abc", NULL);
	TEST_VALIDATE_OK ("/\\\\/abc", NULL);
	TEST_VALIDATE_OK ("/\\\\\\//abc", NULL);
	TEST_VALIDATE_OK ("/\\\\\\\\/abc", NULL);
	TEST_VALIDATE_OK ("/\\\\\\\\\\//abc", NULL);

	TEST_VALIDATE_OK ("user:/\\//abc", NULL);
	TEST_VALIDATE_OK ("user:/\\\\/abc", NULL);
	TEST_VALIDATE_OK ("user:/\\\\\\//abc", NULL);
	TEST_VALIDATE_OK ("user:/\\\\\\\\/abc", NULL);
	TEST_VALIDATE_OK ("user:/\\\\\\\\\\//abc", NULL);

	TEST_VALIDATE_OK ("user:/tests/plugin/\\//abc", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\/abc", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\//abc", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\/abc", NULL);
	TEST_VALIDATE_OK ("user:/tests/plugin/\\\\\\\\\\//abc", NULL);

	TEST_VALIDATE_OK ("/\\///", NULL);
	TEST_VALIDATE_OK ("/\\/\\/", NULL);

	TEST_VALIDATE_OK ("/abc/def/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/def/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/def/ghi/", NULL);
	TEST_VALIDATE_OK ("user:/abc/def/ghi/", NULL);

	TEST_VALIDATE_OK ("/abc//def////ghi/", NULL);
	TEST_VALIDATE_OK ("user://abc////def/ghi/", NULL);

	TEST_VALIDATE_OK ("/////////", NULL);
	TEST_VALIDATE_OK ("user://///////", NULL);

	TEST_VALIDATE_OK ("/a////////", NULL);
	TEST_VALIDATE_OK ("user:/a////////", NULL);

	TEST_VALIDATE_OK ("/abc/%/def", NULL);
	TEST_VALIDATE_OK ("user:/abc/%/def", NULL);

	TEST_VALIDATE_OK ("/abc/d@ef/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/d@ef/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/d%ef/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/d%ef/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/.def/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/.def/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/./ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/./ghi", NULL);

	TEST_VALIDATE_OK ("/abc/../ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/../ghi", NULL);

	TEST_VALIDATE_OK ("/abc/.../ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/.../ghi", NULL);

	TEST_VALIDATE_OK ("/abc/..../ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/..../ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#0/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#0/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#1/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#1/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#5/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#5/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#10/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#10/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#10000/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#10000/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#9223372036854775807/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#9223372036854775807/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#_10/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#_10/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#1/#_10", NULL);
	TEST_VALIDATE_OK ("user:/abc/#1/#_10", NULL);

	TEST_VALIDATE_OK ("/abc/#____10000/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#____10000/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#__________________9223372036854775807/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#__________________9223372036854775807/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/\\%/def", NULL);
	TEST_VALIDATE_OK ("user:/abc/\\%/def", NULL);

	TEST_VALIDATE_OK ("/abc/\\./ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/\\./ghi", NULL);

	TEST_VALIDATE_OK ("/abc/\\../ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/\\../ghi", NULL);

	TEST_VALIDATE_OK ("/abc/\\#10000/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/\\#10000/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/\\#9223372036854775807/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/\\#9223372036854775807/ghi", NULL);

	TEST_VALIDATE_OK ("/abc\\/def/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc\\/def/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/de\\\\f/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/de\\\\f/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/de\\\\\\\\f/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/de\\\\\\\\f/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/de\\\\\\\\\\\\f/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/de\\\\\\\\\\\\f/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/def/ghi\\/", NULL);
	TEST_VALIDATE_OK ("user:/abc/def/ghi\\/", NULL);

	TEST_VALIDATE_OK ("/abc/#/def", NULL);
	TEST_VALIDATE_OK ("user:/abc/#/def", NULL);

	TEST_VALIDATE_OK ("/abc/def/ghi/%", NULL);
	TEST_VALIDATE_OK ("user:/abc/def/ghi/%", NULL);
	TEST_VALIDATE_OK ("user:/abc/..", NULL);

	TEST_VALIDATE_OK ("///////.", NULL);
	TEST_VALIDATE_OK ("/.", NULL);
	TEST_VALIDATE_OK ("///.////.", NULL);
	TEST_VALIDATE_OK ("////.///", NULL);
	TEST_VALIDATE_OK ("//.///.//.", NULL);
	TEST_VALIDATE_OK ("/./a", NULL);

	TEST_VALIDATE_OK ("/\\\\", NULL);

	TEST_VALIDATE_OK ("///////.", "/");
	TEST_VALIDATE_OK ("/.", "/");
	TEST_VALIDATE_OK ("///.////.", "/");
	TEST_VALIDATE_OK ("////.///", "/");
	TEST_VALIDATE_OK ("//.///.//.", "/");
	TEST_VALIDATE_OK ("/./a", "/");

	TEST_VALIDATE_OK ("", "/");
	TEST_VALIDATE_OK ("", "user:/");

	TEST_VALIDATE_OK ("", "/abc");
	TEST_VALIDATE_OK ("/", "/abc");

	TEST_VALIDATE_OK ("/abc/def/ghi", "/abc");
	TEST_VALIDATE_OK ("/abc/def/ghi/", "/abc");
	TEST_VALIDATE_OK ("/abc//def////ghi/", "/abc");
	TEST_VALIDATE_OK ("/////////", "/abc");
	TEST_VALIDATE_OK ("/abc/%/def", "/abc");
	TEST_VALIDATE_OK ("/abc/d@ef/ghi", "/abc");
	TEST_VALIDATE_OK ("/abc/.def/ghi", "/abc");

	TEST_VALIDATE_OK ("abc", "/");
	TEST_VALIDATE_OK ("abc", "/abc");
	TEST_VALIDATE_OK (".", "/abc");

	TEST_VALIDATE_OK ("../abc", "/x");
	TEST_VALIDATE_OK ("../../ab/c", "/x/y");

	TEST_VALIDATE_OK ("/..", "/abc");
	TEST_VALIDATE_OK ("..", "/abc");
	TEST_VALIDATE_OK ("/../..", "/abc/def");
	TEST_VALIDATE_OK ("../..", "/abc/def");
	TEST_VALIDATE_OK ("/def/../..", "/abc");
	TEST_VALIDATE_OK ("def/../..", "/abc");
	TEST_VALIDATE_OK ("/%/../..", "/abc");
	TEST_VALIDATE_OK ("%/../..", "/abc");

	TEST_VALIDATE_OK ("/..///////////////..", "/abc/def");

	TEST_VALIDATE_OK ("..", "/%");
	TEST_VALIDATE_OK ("..", "/\\/");
	TEST_VALIDATE_OK ("/\\//..", "/abc");
	TEST_VALIDATE_OK ("/\\./..", "/abc");
	TEST_VALIDATE_OK ("/a/./..", "/abc");
	TEST_VALIDATE_OK ("/\\\\\\\\\\\\/..", "/abc");

	TEST_VALIDATE_OK ("../.", "/%");

	TEST_VALIDATE_OK ("/#0/..", "/");
	TEST_VALIDATE_OK ("/#10/..", "/");
	TEST_VALIDATE_OK ("/#_10/..", "/");

	TEST_VALIDATE_OK ("user:", "/");
	TEST_VALIDATE_OK ("user", "/");

	TEST_VALIDATE_OK ("abc", "user:/");
	TEST_VALIDATE_OK ("abc", "user:/abc");
	TEST_VALIDATE_OK ("..", "user:/abc");

	TEST_VALIDATE_OK ("/./../..", "/abc");
	TEST_VALIDATE_OK ("./../..", "/abc");
	TEST_VALIDATE_OK ("abc/..", "user:/");

	TEST_VALIDATE_OK ("/abc/%def/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/%def/ghi", NULL);

	TEST_VALIDATE_OK ("/..", NULL);
	TEST_VALIDATE_OK ("user:/..", NULL);
	TEST_VALIDATE_OK ("/abc/../..", NULL);

	TEST_VALIDATE_OK ("/../..", "/abc");
	TEST_VALIDATE_OK ("@", "/abc");

	TEST_VALIDATE_OK ("/abc/#92233720368547758071/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#92233720368547758071/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#9223372036854775808/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#9223372036854775808/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#__________________9223372036854775808/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#__________________9223372036854775808/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#01/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#01/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#__10/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#__10/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#_100/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#_100/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/#___10/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/#___10/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/@/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/@/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/@def/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/@def/ghi", NULL);

	TEST_VALIDATE_OK ("/abc/\\#10/ghi", NULL);
	TEST_VALIDATE_OK ("user:/abc/\\#10/ghi", NULL);

	TEST_VALIDATE_OK ("/./../../..", "/abc");
	TEST_VALIDATE_OK ("./../../..", "/abc");

	TEST_VALIDATE_OK ("..", "/");
	TEST_VALIDATE_OK ("../..", "/a\\/b");
	TEST_VALIDATE_OK ("..", "user:/");

	TEST_VALIDATE_OK ("../user", "user:/");
	TEST_VALIDATE_OK ("../../../../../../..//user", "/much/more/level/1/2/3");
	TEST_VALIDATE_OK ("..///../../../../../../..//user", "/much/more/level/1/2/3");
	TEST_VALIDATE_OK ("..///../../..////../../../..//user", "/much/more/level/1/2/3");
	TEST_VALIDATE_OK ("../../....///../../..////../../../..//user", "/much/more/level/1/2/3");

	TEST_VALIDATE_OK ("/%a", NULL);
	TEST_VALIDATE_OK ("user:/%a", NULL);
	TEST_VALIDATE_OK ("%a", "/");
	TEST_VALIDATE_OK ("%a", "user:/");
	TEST_VALIDATE_OK ("%", "user:/abc");
	TEST_VALIDATE_OK ("..", "user:/");
	TEST_VALIDATE_OK ("..", "system:/elektra/mountpoints/system:\\/info\\/elektra\\/constants");
	TEST_VALIDATE_OK ("..", "system:/elektra/mountpoints");
	TEST_VALIDATE_OK ("..", "system:/elektra");

	succeed_if (!elektraKeyNameValidate (NULL, true), "(NULL) SHOULD NOT BE a valid complete key name");

	TEST_VALIDATE_ERROR ("", NULL);
	TEST_VALIDATE_ERROR ("user/", NULL);
	TEST_VALIDATE_ERROR ("user:", NULL);
	TEST_VALIDATE_ERROR ("user", NULL);

	TEST_VALIDATE_ERROR ("/\\", NULL);
	TEST_VALIDATE_ERROR ("/\\\\\\", NULL);

	TEST_VALIDATE_ERROR ("user:abc:/", NULL);
	TEST_VALIDATE_ERROR ("abc:/", NULL);
	TEST_VALIDATE_ERROR ("abc", NULL);

	TEST_VALIDATE_ERROR ("/abc/\\def/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/\\def/ghi", NULL);

	TEST_VALIDATE_ERROR ("/abc/d\\.ef/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/d\\.ef/ghi", NULL);

	TEST_VALIDATE_ERROR ("/abc/d\\#ef/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/d\\#ef/ghi", NULL);

	TEST_VALIDATE_ERROR ("/abc/d\\%ef/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/d\\%ef/ghi", NULL);

	TEST_VALIDATE_ERROR ("/abc/d\\@ef/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/d\\@ef/ghi", NULL);

	TEST_VALIDATE_ERROR ("/abc/\\def/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/\\def/ghi", NULL);

	TEST_VALIDATE_ERROR ("/\\\\\\%", NULL);
	TEST_VALIDATE_ERROR ("/\\\\\\\\\\%", NULL);

	TEST_VALIDATE_ERROR ("/abc/\\#_10/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/\\#_10/ghi", NULL);

	TEST_VALIDATE_ERROR ("/abc/\\#0/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/\\#0/ghi", NULL);

	TEST_VALIDATE_ERROR ("/abc/\\.def/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/\\.def/ghi", NULL);

	TEST_VALIDATE_ERROR ("/abc/\\#01/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/\\#01/ghi", NULL);

	TEST_VALIDATE_ERROR ("/abc/\\@/def", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/\\@/def", NULL);

	TEST_VALIDATE_ERROR ("/abc/\\#def/ghi", NULL);
	TEST_VALIDATE_ERROR ("user:/abc/\\#def/ghi", NULL);

	TEST_VALIDATE_ERROR ("/\\#0/\\#1", NULL);
	TEST_VALIDATE_ERROR ("/\\#0/..", "/");

	TEST_VALIDATE_OK ("/%", NULL);
	TEST_VALIDATE_OK ("//%", NULL);
	TEST_VALIDATE_OK ("///%", NULL);
	TEST_VALIDATE_OK ("user:/%", NULL);
	TEST_VALIDATE_OK ("user://%", NULL);
	TEST_VALIDATE_OK ("user:///%", NULL);
	TEST_VALIDATE_OK ("%", "/");
	TEST_VALIDATE_OK ("%", "user:/");
}

#undef TEST_VALIDATE_OK
#undef TEST_VALIDATE_ERROR

#define TEST_CANONICALIZE_OK(name, prefix, cname, usizeOld, usizeNew)                                                                      \
	do                                                                                                                                 \
	{                                                                                                                                  \
		size_t csize = prefix == NULL ? 0 : sizeof (prefix);                                                                       \
		size_t usize = usizeOld;                                                                                                   \
		char * buf = prefix == NULL ? NULL : elektraStrDup (prefix);                                                               \
		elektraKeyNameCanonicalize (name, &buf, &csize, csize, &usize);                                                            \
		succeed_if_same_string (buf, cname);                                                                                       \
		succeed_if_fmt (csize == sizeof (cname), "'%s' + '%s': unescaped size wrong (act != exp): %zu != %zu",                     \
				prefix == NULL ? "" : prefix, name, csize, sizeof (cname));                                                \
		succeed_if_fmt (usize == usizeNew, "'%s' + '%s': unescaped size wrong (act != exp): %zu != %zu",                           \
				prefix == NULL ? "" : prefix, name, usize, (size_t) usizeNew);                                             \
		elektraFree (buf);                                                                                                         \
	} while (0)

static void test_canonicalize (void)
{
	TEST_CANONICALIZE_OK ("/", NULL, "/", 0, 3);
	TEST_CANONICALIZE_OK ("proc:/", NULL, "proc:/", 0, 3);
	TEST_CANONICALIZE_OK ("dir:/", NULL, "dir:/", 0, 3);
	TEST_CANONICALIZE_OK ("user:/", NULL, "user:/", 0, 3);
	TEST_CANONICALIZE_OK ("system:/", NULL, "system:/", 0, 3);
	TEST_CANONICALIZE_OK ("spec:/", NULL, "spec:/", 0, 3);
	TEST_CANONICALIZE_OK ("meta:/", NULL, "meta:/", 0, 3);
	TEST_CANONICALIZE_OK ("default:/", NULL, "default:/", 0, 3);

	TEST_CANONICALIZE_OK ("/abc/def/ghi", NULL, "/abc/def/ghi", 0, 14);
	TEST_CANONICALIZE_OK ("user:/abc/def/ghi", NULL, "user:/abc/def/ghi", 0, 14);

	TEST_CANONICALIZE_OK ("/abc/def/ghi/", NULL, "/abc/def/ghi", 0, 14);
	TEST_CANONICALIZE_OK ("user:/abc/def/ghi/", NULL, "user:/abc/def/ghi", 0, 14);

	TEST_CANONICALIZE_OK ("/abc//def////ghi/", NULL, "/abc/def/ghi", 0, 14);
	TEST_CANONICALIZE_OK ("user://abc////def/ghi/", NULL, "user:/abc/def/ghi", 0, 14);

	TEST_CANONICALIZE_OK ("/////////", NULL, "/", 0, 3);
	TEST_CANONICALIZE_OK ("user://///////", NULL, "user:/", 0, 3);

	TEST_CANONICALIZE_OK ("/abc/%/def", NULL, "/abc/%/def", 0, 11);
	TEST_CANONICALIZE_OK ("user:/abc/%/def", NULL, "user:/abc/%/def", 0, 11);

	TEST_CANONICALIZE_OK ("/abc/d@ef/ghi", NULL, "/abc/d@ef/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/d@ef/ghi", NULL, "user:/abc/d@ef/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/.def/ghi", NULL, "/abc/.def/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/.def/ghi", NULL, "user:/abc/.def/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/./ghi", NULL, "/abc/ghi", 0, 10);
	TEST_CANONICALIZE_OK ("user:/abc/./ghi", NULL, "user:/abc/ghi", 0, 10);

	TEST_CANONICALIZE_OK ("/abc/def/../ghi", NULL, "/abc/ghi", 0, 10);
	TEST_CANONICALIZE_OK ("user:/abc/def/../ghi", NULL, "user:/abc/ghi", 0, 10);

	TEST_CANONICALIZE_OK ("/abc/../ghi", NULL, "/ghi", 0, 6);
	TEST_CANONICALIZE_OK ("user:/abc/../ghi", NULL, "user:/ghi", 0, 6);

	TEST_CANONICALIZE_OK ("/abc/.../ghi", NULL, "/abc/.../ghi", 0, 14);
	TEST_CANONICALIZE_OK ("user:/abc/.../ghi", NULL, "user:/abc/.../ghi", 0, 14);

	TEST_CANONICALIZE_OK ("/abc/..../ghi", NULL, "/abc/..../ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/..../ghi", NULL, "user:/abc/..../ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/#0/ghi", NULL, "/abc/#0/ghi", 0, 13);
	TEST_CANONICALIZE_OK ("user:/abc/#0/ghi", NULL, "user:/abc/#0/ghi", 0, 13);

	TEST_CANONICALIZE_OK ("/abc/#1/ghi", NULL, "/abc/#1/ghi", 0, 13);
	TEST_CANONICALIZE_OK ("user:/abc/#1/ghi", NULL, "user:/abc/#1/ghi", 0, 13);

	TEST_CANONICALIZE_OK ("/abc/#5/ghi", NULL, "/abc/#5/ghi", 0, 13);
	TEST_CANONICALIZE_OK ("user:/abc/#5/ghi", NULL, "user:/abc/#5/ghi", 0, 13);

	TEST_CANONICALIZE_OK ("/abc/#10/ghi", NULL, "/abc/#_10/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/#10/ghi", NULL, "user:/abc/#_10/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/#10000/ghi", NULL, "/abc/#____10000/ghi", 0, 21);
	TEST_CANONICALIZE_OK ("user:/abc/#10000/ghi", NULL, "user:/abc/#____10000/ghi", 0, 21);

	TEST_CANONICALIZE_OK ("/abc/#9223372036854775807/ghi", NULL, "/abc/#__________________9223372036854775807/ghi", 0, 49);
	TEST_CANONICALIZE_OK ("user:/abc/#9223372036854775807/ghi", NULL, "user:/abc/#__________________9223372036854775807/ghi", 0, 49);

	TEST_CANONICALIZE_OK ("/abc/#_10/ghi", NULL, "/abc/#_10/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/#_10/ghi", NULL, "user:/abc/#_10/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/#____10000/ghi", NULL, "/abc/#____10000/ghi", 0, 21);
	TEST_CANONICALIZE_OK ("user:/abc/#____10000/ghi", NULL, "user:/abc/#____10000/ghi", 0, 21);

	TEST_CANONICALIZE_OK ("/abc/#__________________9223372036854775807/ghi", NULL, "/abc/#__________________9223372036854775807/ghi", 0,
			      49);
	TEST_CANONICALIZE_OK ("user:/abc/#__________________9223372036854775807/ghi", NULL,
			      "user:/abc/#__________________9223372036854775807/ghi", 0, 49);

	TEST_CANONICALIZE_OK ("/abc/#10/ghi", NULL, "/abc/#_10/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/#10/ghi", NULL, "user:/abc/#_10/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/#1/#_10", NULL, "/abc/#1/#_10", 0, 14);
	TEST_CANONICALIZE_OK ("user:/abc/#1/#_10", NULL, "user:/abc/#1/#_10", 0, 14);

	TEST_CANONICALIZE_OK ("/abc/\\%/def", NULL, "/abc/\\%/def", 0, 12);
	TEST_CANONICALIZE_OK ("user:/abc/\\%/def", NULL, "user:/abc/\\%/def", 0, 12);

	TEST_CANONICALIZE_OK ("/abc/\\./ghi", NULL, "/abc/\\./ghi", 0, 12);
	TEST_CANONICALIZE_OK ("user:/abc/\\./ghi", NULL, "user:/abc/\\./ghi", 0, 12);

	TEST_CANONICALIZE_OK ("/abc/\\../ghi", NULL, "/abc/\\../ghi", 0, 13);
	TEST_CANONICALIZE_OK ("user:/abc/\\../ghi", NULL, "user:/abc/\\../ghi", 0, 13);

	TEST_CANONICALIZE_OK ("/abc/\\#10/ghi", NULL, "/abc/\\#10/ghi", 0, 14);
	TEST_CANONICALIZE_OK ("user:/abc/\\#10/ghi", NULL, "user:/abc/\\#10/ghi", 0, 14);

	TEST_CANONICALIZE_OK ("/abc/\\#10000/ghi", NULL, "/abc/\\#10000/ghi", 0, 17);
	TEST_CANONICALIZE_OK ("user:/abc/\\#10000/ghi", NULL, "user:/abc/\\#10000/ghi", 0, 17);

	TEST_CANONICALIZE_OK ("/abc/\\#9223372036854775807/ghi", NULL, "/abc/\\#9223372036854775807/ghi", 0, 31);
	TEST_CANONICALIZE_OK ("user:/abc/\\#9223372036854775807/ghi", NULL, "user:/abc/\\#9223372036854775807/ghi", 0, 31);

	TEST_CANONICALIZE_OK ("/abc\\/def/ghi", NULL, "/abc\\/def/ghi", 0, 14);
	TEST_CANONICALIZE_OK ("user:/abc\\/def/ghi", NULL, "user:/abc\\/def/ghi", 0, 14);

	TEST_CANONICALIZE_OK ("/abc/de\\\\f/ghi", NULL, "/abc/de\\\\f/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/de\\\\f/ghi", NULL, "user:/abc/de\\\\f/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/def/ghi\\/", NULL, "/abc/def/ghi\\/", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/def/ghi\\/", NULL, "user:/abc/def/ghi\\/", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/\\@/def", NULL, "/abc/\\@/def", 0, 12);
	TEST_CANONICALIZE_OK ("user:/abc/\\@/def", NULL, "user:/abc/\\@/def", 0, 12);

	TEST_CANONICALIZE_OK ("/abc/#/def", NULL, "/abc/#/def", 0, 12);
	TEST_CANONICALIZE_OK ("user:/abc/#/def", NULL, "user:/abc/#/def", 0, 12);

	TEST_CANONICALIZE_OK ("/abc/\\#123/ghi", NULL, "/abc/\\#123/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/\\#123/ghi", NULL, "user:/abc/\\#123/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/\\#def/ghi", NULL, "/abc/\\#def/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/\\#def/ghi", NULL, "user:/abc/\\#def/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("", "/abc", "/abc", 6, 6);
	TEST_CANONICALIZE_OK ("/", "/abc", "/abc", 6, 6);
	TEST_CANONICALIZE_OK ("%", "/abc", "/abc/%", 6, 7);

	TEST_CANONICALIZE_OK ("", "/", "/", 3, 3);
	TEST_CANONICALIZE_OK ("/", "/", "/", 3, 3);

	TEST_CANONICALIZE_OK ("", "user:/", "user:/", 3, 3);
	TEST_CANONICALIZE_OK ("/", "user:/", "user:/", 3, 3);

	TEST_CANONICALIZE_OK ("/abc/def/ghi", "/abc", "/abc/abc/def/ghi", 6, 18);
	TEST_CANONICALIZE_OK ("/abc/def/ghi/", "/abc", "/abc/abc/def/ghi", 6, 18);
	TEST_CANONICALIZE_OK ("/abc//def////ghi/", "/abc", "/abc/abc/def/ghi", 6, 18);
	TEST_CANONICALIZE_OK ("/////////", "/abc", "/abc", 6, 6);
	TEST_CANONICALIZE_OK ("/abc/%/def", "/abc", "/abc/abc/%/def", 6, 15);
	TEST_CANONICALIZE_OK ("/abc/d@ef/ghi", "/abc", "/abc/abc/d@ef/ghi", 6, 19);
	TEST_CANONICALIZE_OK ("/abc/.def/ghi", "/abc", "/abc/abc/.def/ghi", 6, 19);

	TEST_CANONICALIZE_OK ("abc", "/", "/abc", 3, 6);
	TEST_CANONICALIZE_OK ("abc", "/abc", "/abc/abc", 6, 10);
	TEST_CANONICALIZE_OK ("..", "/abc", "/", 6, 3);
	TEST_CANONICALIZE_OK ("../", "/abc", "/", 6, 3);
	TEST_CANONICALIZE_OK ("/../..", "/abc/def", "/", 10, 3);
	TEST_CANONICALIZE_OK ("../..", "/abc/def", "/", 10, 3);

	TEST_CANONICALIZE_OK ("user/", "/", "/user", 3, 7);
	TEST_CANONICALIZE_OK ("user:", "/", "/user:", 3, 8);
	TEST_CANONICALIZE_OK ("user", "/", "/user", 3, 7);

	TEST_CANONICALIZE_OK ("..", "user:/abc", "user:/", 6, 3);
	TEST_CANONICALIZE_OK ("abc/..", "user:/", "user:/", 3, 3);
	TEST_CANONICALIZE_OK ("user:/abc/..", NULL, "user:/", 0, 3);

	TEST_CANONICALIZE_OK ("/\\%", NULL, "/\\%", 0, 4);
	TEST_CANONICALIZE_OK ("/\\\\%", NULL, "/\\\\%", 0, 5);
	TEST_CANONICALIZE_OK ("/\\\\\\\\%", NULL, "/\\\\\\\\%", 0, 6);

	TEST_CANONICALIZE_OK ("user:/\\%", NULL, "user:/\\%", 0, 4);
	TEST_CANONICALIZE_OK ("user:/\\\\%", NULL, "user:/\\\\%", 0, 5);
	TEST_CANONICALIZE_OK ("user:/\\\\\\\\%", NULL, "user:/\\\\\\\\%", 0, 6);

	TEST_CANONICALIZE_OK ("/\\/", NULL, "/\\/", 0, 4);
	TEST_CANONICALIZE_OK ("/\\\\", NULL, "/\\\\", 0, 4);
	TEST_CANONICALIZE_OK ("/\\\\\\/", NULL, "/\\\\\\/", 0, 5);
	TEST_CANONICALIZE_OK ("/\\\\\\\\", NULL, "/\\\\\\\\", 0, 5);
	TEST_CANONICALIZE_OK ("/\\\\\\\\\\/", NULL, "/\\\\\\\\\\/", 0, 6);

	TEST_CANONICALIZE_OK ("/\\//", NULL, "/\\/", 0, 4);
	TEST_CANONICALIZE_OK ("/\\\\/", NULL, "/\\\\", 0, 4);
	TEST_CANONICALIZE_OK ("/\\\\\\//", NULL, "/\\\\\\/", 0, 5);
	TEST_CANONICALIZE_OK ("/\\\\\\\\/", NULL, "/\\\\\\\\", 0, 5);
	TEST_CANONICALIZE_OK ("/\\\\\\\\\\//", NULL, "/\\\\\\\\\\/", 0, 6);

	TEST_CANONICALIZE_OK ("user:/\\/", NULL, "user:/\\/", 0, 4);
	TEST_CANONICALIZE_OK ("user:/\\\\", NULL, "user:/\\\\", 0, 4);
	TEST_CANONICALIZE_OK ("user:/\\\\\\/", NULL, "user:/\\\\\\/", 0, 5);
	TEST_CANONICALIZE_OK ("user:/\\\\\\\\", NULL, "user:/\\\\\\\\", 0, 5);
	TEST_CANONICALIZE_OK ("user:/\\\\\\\\\\/", NULL, "user:/\\\\\\\\\\/", 0, 6);

	TEST_CANONICALIZE_OK ("user:/\\//", NULL, "user:/\\/", 0, 4);
	TEST_CANONICALIZE_OK ("user:/\\\\/", NULL, "user:/\\\\", 0, 4);
	TEST_CANONICALIZE_OK ("user:/\\\\\\//", NULL, "user:/\\\\\\/", 0, 5);
	TEST_CANONICALIZE_OK ("user:/\\\\\\\\/", NULL, "user:/\\\\\\\\", 0, 5);
	TEST_CANONICALIZE_OK ("user:/\\\\\\\\\\//", NULL, "user:/\\\\\\\\\\/", 0, 6);

	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\/", NULL, "user:/tests/plugin/\\/", 0, 17);
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\", NULL, "user:/tests/plugin/\\\\", 0, 17);
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\/", NULL, "user:/tests/plugin/\\\\\\/", 0, 18);
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\\\", NULL, "user:/tests/plugin/\\\\\\\\", 0, 18);
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\\\\\/", NULL, "user:/tests/plugin/\\\\\\\\\\/", 0, 19);

	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\//", NULL, "user:/tests/plugin/\\/", 0, 17);
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\/", NULL, "user:/tests/plugin/\\\\", 0, 17);
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\//", NULL, "user:/tests/plugin/\\\\\\/", 0, 18);
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\\\/", NULL, "user:/tests/plugin/\\\\\\\\", 0, 18);
	TEST_CANONICALIZE_OK ("user:/tests/plugin/\\\\\\\\\\//", NULL, "user:/tests/plugin/\\\\\\\\\\/", 0, 19);

	TEST_CANONICALIZE_OK ("/abc/%def/ghi", NULL, "/abc/%def/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("/abc/%d%ef%/ghi", NULL, "/abc/%d%ef%/ghi", 0, 17);
	TEST_CANONICALIZE_OK ("/abc/%def%/ghi", NULL, "/abc/%def%/ghi", 0, 16);
	TEST_CANONICALIZE_OK ("/abc/d%ef/ghi", NULL, "/abc/d%ef/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/%def/ghi", NULL, "/abc/%def/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/%def/ghi", NULL, "user:/abc/%def/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/..", NULL, "/", 0, 3);
	TEST_CANONICALIZE_OK ("user:/..", NULL, "user:/", 0, 3);
	TEST_CANONICALIZE_OK ("/abc/../..", NULL, "/", 0, 3);

	TEST_CANONICALIZE_OK ("/../..", "/abc", "/", 6, 3);
	TEST_CANONICALIZE_OK ("@", "/abc", "/abc/@", 8, 10);

	TEST_CANONICALIZE_OK ("/abc/#92233720368547758071/ghi", NULL, "/abc/#92233720368547758071/ghi", 0, 32);
	TEST_CANONICALIZE_OK ("user:/abc/#92233720368547758071/ghi", NULL, "user:/abc/#92233720368547758071/ghi", 0, 32);

	TEST_CANONICALIZE_OK ("/abc/#9223372036854775808/ghi", NULL, "/abc/#9223372036854775808/ghi", 0, 31);
	TEST_CANONICALIZE_OK ("user:/abc/#9223372036854775808/ghi", NULL, "user:/abc/#9223372036854775808/ghi", 0, 31);

	TEST_CANONICALIZE_OK ("/abc/#__________________9223372036854775808/ghi", NULL, "/abc/#__________________9223372036854775808/ghi", 0,
			      49);
	TEST_CANONICALIZE_OK ("user:/abc/#__________________9223372036854775808/ghi", NULL,
			      "user:/abc/#__________________9223372036854775808/ghi", 0, 49);

	TEST_CANONICALIZE_OK ("/abc/#01/ghi", NULL, "/abc/#01/ghi", 0, 14);
	TEST_CANONICALIZE_OK ("user:/abc/#01/ghi", NULL, "user:/abc/#01/ghi", 0, 14);

	TEST_CANONICALIZE_OK ("/abc/#__10/ghi", NULL, "/abc/#__10/ghi", 0, 16);
	TEST_CANONICALIZE_OK ("user:/abc/#__10/ghi", NULL, "user:/abc/#__10/ghi", 0, 16);

	TEST_CANONICALIZE_OK ("/abc/#_100/ghi", NULL, "/abc/#_100/ghi", 0, 16);
	TEST_CANONICALIZE_OK ("user:/abc/#_100/ghi", NULL, "user:/abc/#_100/ghi", 0, 16);

	TEST_CANONICALIZE_OK ("/abc/#___10/ghi", NULL, "/abc/#___10/ghi", 0, 17);
	TEST_CANONICALIZE_OK ("user:/abc/#___10/ghi", NULL, "user:/abc/#___10/ghi", 0, 17);

	TEST_CANONICALIZE_OK ("/abc/@/ghi", NULL, "/abc/@/ghi", 0, 12);
	TEST_CANONICALIZE_OK ("user:/abc/@/ghi", NULL, "user:/abc/@/ghi", 0, 12);

	TEST_CANONICALIZE_OK ("/abc/@def/ghi", NULL, "/abc/@def/ghi", 0, 15);
	TEST_CANONICALIZE_OK ("user:/abc/@def/ghi", NULL, "user:/abc/@def/ghi", 0, 15);

	TEST_CANONICALIZE_OK ("/abc/\\#10/ghi", NULL, "/abc/\\#10/ghi", 0, 14);
	TEST_CANONICALIZE_OK ("user:/abc/\\#10/ghi", NULL, "user:/abc/\\#10/ghi", 0, 14);

	TEST_CANONICALIZE_OK ("/./../../..", "/abc", "/", 6, 3);
	TEST_CANONICALIZE_OK ("./../../..", "/abc", "/", 6, 3);

	TEST_CANONICALIZE_OK ("..", "/", "/", 3, 3);
	TEST_CANONICALIZE_OK ("../..", "/a\\/b", "/", 6, 3);
	TEST_CANONICALIZE_OK ("..", "user:/", "user:/", 3, 3);
	TEST_CANONICALIZE_OK ("..", "user:/abc", "user:/", 6, 3);
	TEST_CANONICALIZE_OK ("..", "/abc", "/", 6, 3);

	TEST_CANONICALIZE_OK ("../user", "user:/", "user:/user", 3, 7);
	TEST_CANONICALIZE_OK ("../../../../../../..//user", "/much/more/level/1/2/3", "/user", 24, 7);
	TEST_CANONICALIZE_OK ("..///../../../../../../..//user", "/much/more/level/1/2/3", "/user", 24, 7);
	TEST_CANONICALIZE_OK ("..///../../..////../../../..//user", "/much/more/level/1/2/3", "/user", 24, 7);
	TEST_CANONICALIZE_OK ("../../....///../../..////../../../..//user", "/much/more/level/1/2/3", "/user", 24, 7);

	TEST_CANONICALIZE_OK ("..", "system:/elektra/mountpoints/system:\\/info\\/elektra\\/constants", "system:/elektra/mountpoints", 53,
			      22);
	TEST_CANONICALIZE_OK ("..", "system:/elektra/mountpoints", "system:/elektra", 22, 10);
	TEST_CANONICALIZE_OK ("..", "system:/elektra", "system:/", 10, 3);

	/* FIXME: broken, see #3902
		TEST_CANONICALIZE_OK ("/%", NULL, "/%", 0, 4);
		TEST_CANONICALIZE_OK ("//%", NULL, "/%", 0, 4);
		TEST_CANONICALIZE_OK ("///%", NULL, "/%", 0, 4);
		TEST_CANONICALIZE_OK ("user:/%", NULL, "user:/%", 0, 4);
		TEST_CANONICALIZE_OK ("user://%", NULL, "user:/%", 0, 4);
		TEST_CANONICALIZE_OK ("user:///%", NULL, "user:/%", 0, 4);
		TEST_CANONICALIZE_OK ("%", "/", "/%", 3, 4);
		TEST_CANONICALIZE_OK ("%", "user:/", "/%", 3, 4);
	*/
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
		elektraKeyNameUnescape (name, buf);                                                                                        \
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

	TEST_UNESCAPE_OK ("/abc/\\.def/ghi", KEY_NS_CASCADING, "\0abc\0.def\0ghi");
	TEST_UNESCAPE_OK ("user:/abc/\\.def/ghi", KEY_NS_USER, "\0abc\0.def\0ghi");

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

	TEST_UNESCAPE_OK ("/abc/\\%def/ghi", KEY_NS_CASCADING, "\0abc\0%def\0ghi");
	TEST_UNESCAPE_OK ("/abc/\\%d%ef%/ghi", KEY_NS_CASCADING, "\0abc\0%d%ef%\0ghi");
	TEST_UNESCAPE_OK ("/abc/\\%def%/ghi", KEY_NS_CASCADING, "\0abc\0%def%\0ghi");

	/* FIXME: broken, see #3902
		TEST_UNESCAPE_OK ("/%", KEY_NS_CASCADING, "\0\0");
		TEST_UNESCAPE_OK ("user:/%", KEY_NS_USER, "\0\0");
	*/
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
	TEST_ESCAPE_PART_OK ("@", "@");
	TEST_ESCAPE_PART_OK ("#", "#");
	TEST_ESCAPE_PART_OK ("#def", "#def");
	TEST_ESCAPE_PART_OK ("#123", "\\#123");
	TEST_ESCAPE_PART_OK ("#__10", "#__10");
	TEST_ESCAPE_PART_OK ("#_100", "#_100");
	TEST_ESCAPE_PART_OK ("#01", "#01");
	TEST_ESCAPE_PART_OK ("#9223372036854775807", "\\#9223372036854775807");
	TEST_ESCAPE_PART_OK ("#9223372036854775808", "#9223372036854775808");
	TEST_ESCAPE_PART_OK ("#92233720368547758071", "#92233720368547758071");
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

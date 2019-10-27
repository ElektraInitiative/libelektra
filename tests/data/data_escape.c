/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

TEST_ESCAPE_PART("a", "a");
TEST_ESCAPE_PART("$", "$");
TEST_ESCAPE_PART("€", "€");
TEST_ESCAPE_PART("\x01", "\x01");
TEST_ESCAPE_PART("\xFF", "\xFF");
TEST_ESCAPE_PART("\xFF\xFF\xFF\xFF", "\xFF\xFF\xFF\xFF");
TEST_ESCAPE_PART("\xFF\xFF/\xFF\xFF", "\xFF\xFF\\/\xFF\xFF");
TEST_ESCAPE_PART("test", "test");
TEST_ESCAPE_PART("test/name", "test\\/name");
TEST_ESCAPE_PART("a/b/c/d/e/f/g/h/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z",
		 "a\\/b\\/c\\/d\\/e\\/f\\/g\\/h\\/j\\/k\\/l\\/m\\/n\\/o\\/p\\/q\\/r\\/s\\/t\\/u\\/v\\/w\\/x\\/y\\/z");

TEST_ESCAPE_PART("", "%");
TEST_ESCAPE_PART("%", "\\%");
TEST_ESCAPE_PART("\\%", "\\\\%");
TEST_ESCAPE_PART("\\\\%", "\\\\\\\\%");
TEST_ESCAPE_PART("\\\\\\%", "\\\\\\\\\\\\%");

TEST_ESCAPE_PART("\\", "\\\\"); // 1->2
TEST_ESCAPE_PART("\\\\", "\\\\\\\\"); // 2 -> 4
TEST_ESCAPE_PART("\\\\\\", "\\\\\\\\\\\\"); // 3 -> 6
TEST_ESCAPE_PART("\\\\\\\\", "\\\\\\\\\\\\\\\\"); // 4 -> 8
TEST_ESCAPE_PART("\\\\\\\\\\", "\\\\\\\\\\\\\\\\\\\\"); // 5 -> 10

TEST_ESCAPE_PART("a\\\\\\", "a\\\\\\\\\\\\"); // 3 -> 6
TEST_ESCAPE_PART("a/test", "a\\/test");
TEST_ESCAPE_PART("a\\/test", "a\\\\\\/test");

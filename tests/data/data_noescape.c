/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

// clang-format off

TEST_NOESCAPE_PART("a", "a");
TEST_NOESCAPE_PART("$", "$");
TEST_NOESCAPE_PART("€", "€");
TEST_NOESCAPE_PART("\x01", "\x01");
TEST_NOESCAPE_PART("\xFF", "\xFF");
TEST_NOESCAPE_PART("\xFF\xFF\xFF\xFF", "\xFF\xFF\xFF\xFF");
TEST_NOESCAPE_PART("\xFF\xFF/\xFF\xFF", "\xFF\xFF");
TEST_NOESCAPE_PART("test", "test");
TEST_NOESCAPE_PART("test/name", "name");
TEST_NOESCAPE_PART("a/b/c/d/e/f/g/h/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z",
		   "z");
TEST_ESCAPE_PART("a\\/b\\/c\\/d\\/e\\/f\\/g\\/h\\/j\\/k\\/l\\/m\\/n\\/o\\/p\\/q\\/r\\/s\\/t\\/u\\/v\\/w\\/x\\/y\\/z",
		 "a/b/c/d/e/f/g/h/j/k/l/m/n/o/p/q/r/s/t/u/v/w/x/y/z");

TEST_NOESCAPE_PART("%", "");
TEST_NOESCAPE_PART("\\%", "%");
TEST_NOESCAPE_PART("\\\\%", "\\%");
TEST_NOESCAPE_PART("\\\\\\%", "\\\\%");

// TEST_NOESCAPE_PART("a\\\\\\", "a\\\\\\");
// TEST_NOESCAPE_PART("\\", "\\");
// TEST_NOESCAPE_PART("\\\\", "\\\\");
// TEST_NOESCAPE_PART("\\\\\\\\", "\\\\\\\\");
// TEST_NOESCAPE_PART("\\\\\\\\\\\\\\\\", "\\\\\\\\\\\\\\\\");
//
TEST_NOESCAPE_PART("a/test", "test");
TEST_NOESCAPE_PART("a\\/test", "a/test");

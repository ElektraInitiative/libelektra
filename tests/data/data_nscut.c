/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

ksNew(35,
	keyNew("spec:/a", KEY_END),
	keyNew("spec:/a/b", KEY_END),
	keyNew("spec:/a/b/c", KEY_END),
	keyNew("spec:/a/b/c/d", KEY_END),
	keyNew("spec:/a/b/c/d/e", KEY_END),
	keyNew("spec:/a/b/c/e", KEY_END),
	keyNew("spec:/a/b/c/e/d", KEY_END),

	keyNew("proc:/a", KEY_END),
	keyNew("proc:/a/b", KEY_END),
	keyNew("proc:/a/b/c", KEY_END),
	keyNew("proc:/a/b/c/d", KEY_END),
	keyNew("proc:/a/b/c/d/e", KEY_END),
	keyNew("proc:/a/b/c/e", KEY_END),
	keyNew("proc:/a/b/c/e/d", KEY_END),

	keyNew("dir:/a", KEY_END),
	keyNew("dir:/a/b", KEY_END),
	keyNew("dir:/a/b/c", KEY_END),
	keyNew("dir:/a/b/c/d", KEY_END),
	keyNew("dir:/a/b/c/d/e", KEY_END),
	keyNew("dir:/a/b/c/e", KEY_END),
	keyNew("dir:/a/b/c/e/d", KEY_END),

	keyNew("user:/a", KEY_END),
	keyNew("user:/a/b", KEY_END),
	keyNew("user:/a/b/c", KEY_END),
	keyNew("user:/a/b/c/d", KEY_END),
	keyNew("user:/a/b/c/d/e", KEY_END),
	keyNew("user:/a/b/c/e", KEY_END),
	keyNew("user:/a/b/c/e/d", KEY_END),

	keyNew("system:/a", KEY_END),
	keyNew("system:/a/b", KEY_END),
	keyNew("system:/a/b/c", KEY_END),
	keyNew("system:/a/b/c/d", KEY_END),
	keyNew("system:/a/b/c/d/e", KEY_END),
	keyNew("system:/a/b/c/e", KEY_END),
	keyNew("system:/a/b/c/e/d", KEY_END),
	KS_END);

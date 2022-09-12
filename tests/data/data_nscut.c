/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

ksNew(35,
	keyNew("spec:/a", ELEKTRA_KEY_END),
	keyNew("spec:/a/b", ELEKTRA_KEY_END),
	keyNew("spec:/a/b/c", ELEKTRA_KEY_END),
	keyNew("spec:/a/b/c/d", ELEKTRA_KEY_END),
	keyNew("spec:/a/b/c/d/e", ELEKTRA_KEY_END),
	keyNew("spec:/a/b/c/e", ELEKTRA_KEY_END),
	keyNew("spec:/a/b/c/e/d", ELEKTRA_KEY_END),

	keyNew("proc:/a", ELEKTRA_KEY_END),
	keyNew("proc:/a/b", ELEKTRA_KEY_END),
	keyNew("proc:/a/b/c", ELEKTRA_KEY_END),
	keyNew("proc:/a/b/c/d", ELEKTRA_KEY_END),
	keyNew("proc:/a/b/c/d/e", ELEKTRA_KEY_END),
	keyNew("proc:/a/b/c/e", ELEKTRA_KEY_END),
	keyNew("proc:/a/b/c/e/d", ELEKTRA_KEY_END),

	keyNew("dir:/a", ELEKTRA_KEY_END),
	keyNew("dir:/a/b", ELEKTRA_KEY_END),
	keyNew("dir:/a/b/c", ELEKTRA_KEY_END),
	keyNew("dir:/a/b/c/d", ELEKTRA_KEY_END),
	keyNew("dir:/a/b/c/d/e", ELEKTRA_KEY_END),
	keyNew("dir:/a/b/c/e", ELEKTRA_KEY_END),
	keyNew("dir:/a/b/c/e/d", ELEKTRA_KEY_END),

	keyNew("user:/a", ELEKTRA_KEY_END),
	keyNew("user:/a/b", ELEKTRA_KEY_END),
	keyNew("user:/a/b/c", ELEKTRA_KEY_END),
	keyNew("user:/a/b/c/d", ELEKTRA_KEY_END),
	keyNew("user:/a/b/c/d/e", ELEKTRA_KEY_END),
	keyNew("user:/a/b/c/e", ELEKTRA_KEY_END),
	keyNew("user:/a/b/c/e/d", ELEKTRA_KEY_END),

	keyNew("system:/a", ELEKTRA_KEY_END),
	keyNew("system:/a/b", ELEKTRA_KEY_END),
	keyNew("system:/a/b/c", ELEKTRA_KEY_END),
	keyNew("system:/a/b/c/d", ELEKTRA_KEY_END),
	keyNew("system:/a/b/c/d/e", ELEKTRA_KEY_END),
	keyNew("system:/a/b/c/e", ELEKTRA_KEY_END),
	keyNew("system:/a/b/c/e/d", ELEKTRA_KEY_END),
	ELEKTRA_KS_END);

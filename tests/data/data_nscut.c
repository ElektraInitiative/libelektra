/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// clang-format off

elektraKeysetNew(35,
	elektraKeyNew("spec:/a", ELEKTRA_KEY_END),
	elektraKeyNew("spec:/a/b", ELEKTRA_KEY_END),
	elektraKeyNew("spec:/a/b/c", ELEKTRA_KEY_END),
	elektraKeyNew("spec:/a/b/c/d", ELEKTRA_KEY_END),
	elektraKeyNew("spec:/a/b/c/d/e", ELEKTRA_KEY_END),
	elektraKeyNew("spec:/a/b/c/e", ELEKTRA_KEY_END),
	elektraKeyNew("spec:/a/b/c/e/d", ELEKTRA_KEY_END),

	elektraKeyNew("proc:/a", ELEKTRA_KEY_END),
	elektraKeyNew("proc:/a/b", ELEKTRA_KEY_END),
	elektraKeyNew("proc:/a/b/c", ELEKTRA_KEY_END),
	elektraKeyNew("proc:/a/b/c/d", ELEKTRA_KEY_END),
	elektraKeyNew("proc:/a/b/c/d/e", ELEKTRA_KEY_END),
	elektraKeyNew("proc:/a/b/c/e", ELEKTRA_KEY_END),
	elektraKeyNew("proc:/a/b/c/e/d", ELEKTRA_KEY_END),

	elektraKeyNew("dir:/a", ELEKTRA_KEY_END),
	elektraKeyNew("dir:/a/b", ELEKTRA_KEY_END),
	elektraKeyNew("dir:/a/b/c", ELEKTRA_KEY_END),
	elektraKeyNew("dir:/a/b/c/d", ELEKTRA_KEY_END),
	elektraKeyNew("dir:/a/b/c/d/e", ELEKTRA_KEY_END),
	elektraKeyNew("dir:/a/b/c/e", ELEKTRA_KEY_END),
	elektraKeyNew("dir:/a/b/c/e/d", ELEKTRA_KEY_END),

	elektraKeyNew("user:/a", ELEKTRA_KEY_END),
	elektraKeyNew("user:/a/b", ELEKTRA_KEY_END),
	elektraKeyNew("user:/a/b/c", ELEKTRA_KEY_END),
	elektraKeyNew("user:/a/b/c/d", ELEKTRA_KEY_END),
	elektraKeyNew("user:/a/b/c/d/e", ELEKTRA_KEY_END),
	elektraKeyNew("user:/a/b/c/e", ELEKTRA_KEY_END),
	elektraKeyNew("user:/a/b/c/e/d", ELEKTRA_KEY_END),

	elektraKeyNew("system:/a", ELEKTRA_KEY_END),
	elektraKeyNew("system:/a/b", ELEKTRA_KEY_END),
	elektraKeyNew("system:/a/b/c", ELEKTRA_KEY_END),
	elektraKeyNew("system:/a/b/c/d", ELEKTRA_KEY_END),
	elektraKeyNew("system:/a/b/c/d/e", ELEKTRA_KEY_END),
	elektraKeyNew("system:/a/b/c/e", ELEKTRA_KEY_END),
	elektraKeyNew("system:/a/b/c/e/d", ELEKTRA_KEY_END),
	ELEKTRA_KS_END);

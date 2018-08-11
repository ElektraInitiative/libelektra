/**
 * @file
 *
 * @brief Tests for leaf plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "leaf.hpp"

#include <tuple>

#include <kdbmodule.h>
#include <kdbprivate.h>

#include <tests.hpp>

using std::ignore;
using std::tie;

using ckdb::keyNew;

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;

using elektra::splitArrayParentsOther;

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

#define OPEN_PLUGIN(parentName, filepath)                                                                                                  \
	CppKeySet modules{ 0, KS_END };                                                                                                    \
	CppKeySet config{ 0, KS_END };                                                                                                     \
	elektraModulesInit (modules.getKeySet (), 0);                                                                                      \
	CppKey parent{ parentName, KEY_VALUE, filepath, KEY_END };                                                                         \
	Plugin * plugin = elektraPluginOpen ("leaf", modules.getKeySet (), config.getKeySet (), *parent);                                  \
	exit_if_fail (plugin != NULL, "Could not open leaf plugin");

#define CLOSE_PLUGIN()                                                                                                                     \
	ksDel (modules.release ());                                                                                                        \
	config.release ();                                                                                                                 \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules.getKeySet (), 0)

#define PREFIX "user/tests/leaf/"

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

void test_set (CppKeySet keys, CppKeySet expected, int const status = ELEKTRA_PLUGIN_STATUS_SUCCESS)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]"), annotate ("oclint:suppress[empty if statement]"),
			annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	OPEN_PLUGIN (PREFIX, "file/path");

	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), status, "Call of `kdbSet` failed");
	compare_keyset (keys, expected);

	CLOSE_PLUGIN ();
}

void test_get (CppKeySet keys, CppKeySet expected, int const status = ELEKTRA_PLUGIN_STATUS_SUCCESS)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]"), annotate ("oclint:suppress[empty if statement]"),
			annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	OPEN_PLUGIN (PREFIX, "file/path");

	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), status, "Call of `kdbGet` failed");
	compare_keyset (keys, expected);

	CLOSE_PLUGIN ();
}

void test_roundtrip (CppKeySet keys, int const status = ELEKTRA_PLUGIN_STATUS_SUCCESS)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]"), annotate ("oclint:suppress[empty if statement]"),
			annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	CppKeySet input = keys.dup ();

	OPEN_PLUGIN (PREFIX, "file/path");

	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), status, "Call of `kdbSet` failed");
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), status, "Call of `kdbGet` failed");
	compare_keyset (input, keys);

	CLOSE_PLUGIN ();
}

// -- Tests --------------------------------------------------------------------------------------------------------------------------------

TEST (leaf, splitArrayParentsOther)
{
	// clang-format off
	kdb::KeySet input { 10,
		  keyNew (PREFIX "key", KEY_END),
		  keyNew (PREFIX "key/map", KEY_END),
		  keyNew (PREFIX "key/array", KEY_END),
		  keyNew (PREFIX "key/array/#0", KEY_END),
		  keyNew (PREFIX "key/array/#1", KEY_END),
		  keyNew (PREFIX "key/array/#2/nested", KEY_END),
		  keyNew (PREFIX "key/array/#2/nested/#0", KEY_END),
		  keyNew (PREFIX "key/array/#2/nested/#1", KEY_END),
		  keyNew (PREFIX "key/empty/array", KEY_META, "array", "", KEY_END),
		  KS_END };

	CppKeySet expected { 10,
		  keyNew (PREFIX "key/array", KEY_END),
		  keyNew (PREFIX "key/array/#2/nested", KEY_END),
		  keyNew (PREFIX "key/empty/array", KEY_META, "array", "", KEY_END),
		  KS_END };

	// clang-format on

	CppKeySet arrays;
	tie (arrays, ignore) = splitArrayParentsOther (input);
	compare_keyset (expected, arrays);
}

TEST (leaf, basics)
{
	OPEN_PLUGIN ("system/elektra/modules/leaf", "")

	CppKeySet keys{ 0, KS_END };
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Unable to retrieve plugin contract");

	CLOSE_PLUGIN ();
}

TEST (leaf, get)
{
	test_get (
#include "leaf/simple_set.hpp"
		,
#include "leaf/simple_get.hpp"
	);
}

TEST (leaf, set)
{
	test_set (
#include "leaf/empty.hpp"
		,
#include "leaf/empty.hpp"
		, ELEKTRA_PLUGIN_STATUS_NO_UPDATE);

	test_set (
#include "leaf/simple_get.hpp"
		,
#include "leaf/simple_set.hpp"
	);

	test_set (
#include "leaf/extended_get.hpp"
		,
#include "leaf/extended_set.hpp"
	);
}

TEST (leaf, roundtrip)
{
	test_roundtrip (
#include "leaf/simple_get.hpp"
	);
	test_roundtrip (
#include "leaf/extended_get.hpp"
	);
}

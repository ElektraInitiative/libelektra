/**
 * @file
 *
 * @brief Tests for directoryvalue plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include <internal/plugin/load.h>
#include <internal/plugin/struct.h>
#include <internal/pluginload/module.h>

#include <tests.hpp>

#include "./directoryvalue.hpp"
#include "./directoryvalue_delegate.hpp"

using std::tie;

using ckdb::keyNew;
using ckdb::Plugin;

using elektra::increaseArrayIndices;

// -- Macros -------------------------------------------------------------------------------------------------------------------------------

#define OPEN_PLUGIN(parentName, filepath)                                                                                                  \
	kdb::KeySet modules{ 0, KS_END };                                                                                                  \
	kdb::KeySet config{ 0, KS_END };                                                                                                   \
	elektraModulesInit (modules.getKeySet (), 0);                                                                                      \
	kdb::Key parent{ parentName, KEY_VALUE, filepath, KEY_END };                                                                       \
	Plugin * plugin = elektraPluginOpen ("directoryvalue", modules.getKeySet (), config.getKeySet (), *parent);                        \
	exit_if_fail (plugin != NULL, "Could not open directoryvalue plugin");

#define CLOSE_PLUGIN()                                                                                                                     \
	ksDel (modules.release ());                                                                                                        \
	config.release ();                                                                                                                 \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules.getKeySet (), 0)

#define PREFIX "user:/tests/directoryvalue/"

// -- Functions ----------------------------------------------------------------------------------------------------------------------------

void test_set (kdb::KeySet keys, kdb::KeySet expected, int const status = ELEKTRA_PLUGIN_STATUS_SUCCESS)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]"), annotate ("oclint:suppress[empty if statement]"),
			annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	OPEN_PLUGIN (PREFIX, "file/path");

	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), status, "Call of `kdbSet` failed");
	compare_keyset (expected, keys);

	CLOSE_PLUGIN ();
}

void test_get (kdb::KeySet keys, kdb::KeySet expected, int const status = ELEKTRA_PLUGIN_STATUS_SUCCESS)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]"), annotate ("oclint:suppress[empty if statement]"),
			annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	OPEN_PLUGIN (PREFIX, "file/path");

	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), status, "Call of `kdbGet` failed");
	compare_keyset (expected, keys);

	CLOSE_PLUGIN ();
}

void test_roundtrip (kdb::KeySet keys, int const status = ELEKTRA_PLUGIN_STATUS_SUCCESS)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress[high ncss method]"), annotate ("oclint:suppress[empty if statement]"),
			annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	kdb::KeySet input = keys.dup ();

	OPEN_PLUGIN (PREFIX, "file/path");

	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), status, "Call of `kdbSet` failed");
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), status, "Call of `kdbGet` failed");
	compare_keyset (input, keys);

	CLOSE_PLUGIN ();
}

// -- Tests --------------------------------------------------------------------------------------------------------------------------------

TEST (directoryvalue, increaseArrayIndices) //! OCLint (avoid private static members)
#ifdef __llvm__
__attribute__ ((annotate ("oclint:suppress[empty if statement]"), annotate ("oclint:suppress[too few branches in switch statement]]")))
#endif
{
	kdb::KeySet arrayParents{ 10, keyNew (PREFIX "key/array", KEY_END), keyNew (PREFIX "key/array/#2/nested", KEY_END), KS_END };

	kdb::KeySet expectedArrayParents{ 10, keyNew (PREFIX "key/array", KEY_END), keyNew (PREFIX "key/array/#3/nested", KEY_END),
					  KS_END };

	kdb::KeySet arrays{ 10,
			    keyNew (PREFIX "key/array", KEY_END),
			    keyNew (PREFIX "key/array/#0", KEY_END),
			    keyNew (PREFIX "key/array/#1", KEY_END),
			    keyNew (PREFIX "key/array/#2/nested", KEY_END),
			    keyNew (PREFIX "key/array/#2/nested/#0", KEY_END),
			    keyNew (PREFIX "key/array/#2/nested/#1", KEY_END),
			    keyNew (PREFIX "key/array", KEY_END),
			    keyNew (PREFIX "key/array/#0", KEY_END),
			    keyNew (PREFIX "key/array/#1", KEY_END),
			    KS_END };

	kdb::KeySet expectedArrays{ 10,
				    keyNew (PREFIX "key/array", KEY_END),
				    keyNew (PREFIX "key/array/#1", KEY_END),
				    keyNew (PREFIX "key/array/#2", KEY_END),
				    keyNew (PREFIX "key/array/#3/nested", KEY_END),
				    keyNew (PREFIX "key/array/#3/nested/#1", KEY_END),
				    keyNew (PREFIX "key/array/#3/nested/#2", KEY_END),
				    keyNew (PREFIX "key/array", KEY_END),
				    keyNew (PREFIX "key/array/#1", KEY_END),
				    keyNew (PREFIX "key/array/#2", KEY_END),
				    KS_END };

	tie (arrayParents, arrays) = increaseArrayIndices (arrayParents, arrays);
	compare_keyset (expectedArrays, arrays);
	compare_keyset (expectedArrayParents, arrayParents);
}

TEST (directoryvalue, basics) //! OCLint (avoid private static members)
#ifdef __llvm__
__attribute__ ((annotate ("oclint:suppress[empty if statement]"), annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	OPEN_PLUGIN ("system:/elektra/modules/directoryvalue", "")

	kdb::KeySet keys{ 0, KS_END };
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Unable to retrieve plugin contract");

	CLOSE_PLUGIN ();
}

TEST (directoryvalue, get) //! OCLint (avoid private static members)
{
	test_get (
#include "./directoryvalue/simple_set.hpp"
		,
#include "./directoryvalue/simple_get.hpp"
	);
	test_get (
#include "./directoryvalue/arrays_set.hpp"
		,
#include "./directoryvalue/arrays_get.hpp"
	);
	test_get (
#include "./directoryvalue/mixed_set.hpp"
		,
#include "./directoryvalue/mixed_get.hpp"
	);
}

TEST (directoryvalue, set) //! OCLint (avoid private static members)
{
	test_set (
#include "./directoryvalue/empty.hpp"
		,
#include "./directoryvalue/empty.hpp"
		, ELEKTRA_PLUGIN_STATUS_NO_UPDATE);

	test_set (
#include "./directoryvalue/simple_get.hpp"
		,
#include "./directoryvalue/simple_set.hpp"
	);

	test_set (
#include "./directoryvalue/extended_get.hpp"
		,
#include "./directoryvalue/extended_set.hpp"
	);

	test_set (
#include "./directoryvalue/arrays.hpp"
		,
#include "./directoryvalue/arrays_set.hpp"
	);

	test_set (
#include "./directoryvalue/mixed_get.hpp"
		,
#include "./directoryvalue/mixed_set.hpp"
	);
}

TEST (directoryvalue, roundtrip) //! OCLint (avoid private static members)
{
	test_roundtrip (
#include "./directoryvalue/simple_get.hpp"
	);
	test_roundtrip (
#include "./directoryvalue/extended_get.hpp"
	);
	test_roundtrip (
#include "./directoryvalue/arrays_get.hpp"
	);
	test_roundtrip (
#include "./directoryvalue/mixed_get.hpp"
	);
}

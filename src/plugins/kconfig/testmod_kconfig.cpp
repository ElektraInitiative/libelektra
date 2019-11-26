/**
 * @file
 *
 * @brief Tests for kconfig plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "kconfig.hpp"

#include <fstream>
#include <kdbmodule.h>
#include <kdbprivate.h>

#include <tests.h>
#include <tests.hpp>

using ckdb::keyNew;
using ckdb::ksNew;

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;


// BEGIN: COPY UTIL FUNCTIONS FROM testmod_yamlcpp.cpp
#define PREFIX "user/unit_tests/kconfig/"
#define OPEN_PLUGIN(parentName, filepath)                                                                                                  \
	CppKeySet modules{ 0, KS_END };                                                                                                    \
	CppKeySet config{ 0, KS_END };                                                                                                     \
	elektraModulesInit (modules.getKeySet (), 0);                                                                                      \
	CppKey parent{ parentName, KEY_VALUE, filepath, KEY_END };                                                                         \
	Plugin * plugin = elektraPluginOpen ("kconfig", modules.getKeySet (), config.getKeySet (), *parent);                               \
	exit_if_fail (plugin != NULL, "Could not open kconfig plugin")
#define CLOSE_PLUGIN()                                                                                                                     \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules.getKeySet (), 0);                                                                                     \
	ksDel (modules.release ());                                                                                                        \
	config.release ()

void update_parent (CppKeySet expected, string const filepath)
{
	// We replace the value of the parent key of expected keyset, if the header file specifies the value @CONFIG_FILEPATH@.
	// We could also do that via CMake, but the current solution should be easier for now.
	CppKey root = expected.lookup (PREFIX, KDB_O_POP);
	if (root)
	{
		if (root.getString () == "@CONFIG_FILEPATH@") root.setString (filepath);
		expected.append (root);
	}
}

static void test_read (string const & filename, CppKeySet expected, int const status = ELEKTRA_PLUGIN_STATUS_SUCCESS)
#ifdef __llvm__
	__attribute__ ((annotate ("oclint:suppress")))
#endif
{
	string filepath = srcdir_file (filename.c_str ());
	update_parent (expected, filepath);

	OPEN_PLUGIN (PREFIX, filepath.c_str ());

	CppKeySet keys;
	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), status, "Call of `kdbGet` failed");

	compare_keyset (keys, expected);

	CLOSE_PLUGIN ();
}
// END: COPY UTIL FUNCTIONS FROM testmod_yamlcpp.cpp

TEST (kconfig, basics)
{
	CppKeySet modules{ 0, KS_END };
	CppKeySet config{ 0, KS_END };
	CppKeySet keys{ 0, KS_END };
	elektraModulesInit (modules.getKeySet (), 0);

	CppKey parent{ "system/elektra/modules/kconfig", KEY_END };
	Plugin * plugin = elektraPluginOpen ("kconfig", modules.getKeySet (), config.getKeySet (), *parent);
	exit_if_fail (plugin != NULL, "Could not open kconfig plugin"); //! OCLint (empty if, too few branches switch)

	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbGet` failed");

	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_ERROR, "Call of `kdbSet` failed");

	succeed_if_same (plugin->kdbError (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbError` failed");

	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules.getKeySet (), 0);

	ksDel (modules.release ());
	config.release ();
}

TEST (kconfig, simple_file_get)
{
	test_read ("kconfig/simple_examplerc",
#include "kconfig/simple_example.h"
	);
}

TEST (kconfig, simple_file_set)
{
	// LOAD KConfig module
	CppKeySet modules{ 0, KS_END };
	CppKeySet config{ 0, KS_END };
	CppKey pluginParent{ "system/elektra/modules/kconfig", KEY_END };
	elektraModulesInit (modules.getKeySet (), 0);
	Plugin * plugin = elektraPluginOpen ("kconfig", modules.getKeySet (), config.getKeySet (), pluginParent.getKey ());
	exit_if_fail (plugin != NULL, "Could not open kconfig plugin"); //! OCLint (empty if, too few branches switch)

	// Create parent key
	string filePath = elektraFilename ();
	CppKey parent{ "user/namespace", KEY_END };
	parent.setString (filePath);

	// Create KeySet that we want to save
	CppKey key1{ "user/namespace/group/title", KEY_VALUE, "KConfig Test", KEY_END };
	CppKey key2{ "user/namespace/group/key[en]", KEY_VALUE, "Hello", KEY_META, "kconfig", "ei", KEY_END };
	CppKey key3{ "user/namespace/group/key[de]", KEY_VALUE, "Hallo", KEY_END };
	CppKeySet keys;
	keys.append (key1);
	keys.append (key2);
	keys.append (key3);


	// Save the KeySet to the file stored in the parent key
	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), parent.getKey ()), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Call of `kdbSet` failed");


	// Load the file and verify that it has the correct format
	std::ifstream file{ filePath };
	std::string tmp;

	getline (file, tmp);
	succeed_if_same (tmp, "[group]", "");
	getline (file, tmp);
	succeed_if_same (tmp, "key[de]=Hallo", "");
	getline (file, tmp);
	succeed_if_same (tmp, "key[en][$e][$i]=Hello", "");
	getline (file, tmp);
	succeed_if_same (tmp, "title=KConfig Test", "");


	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules.getKeySet (), 0);

	ksDel (modules.release ());
	config.release ();
}

TEST (kconfig, meta_file_get)
{
	test_read ("kconfig/meta_examplerc",
#include "kconfig/meta_example.h"
	);
}


// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char * argv[])
{
	init (argc, argv); // Required for `srcdir_file` to work properly
	::testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}

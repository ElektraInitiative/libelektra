/**
 * @file
 *
 * @brief Tests for kconfig plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "kconfig.hpp"

#include <elektra/kdbmodule.h>
#include <elektra/kdbprivate.h>
#include <fstream>

#include <tests.h>
#include <tests.hpp>

using ckdb::keyNew;
using ckdb::ksNew;

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;


// BEGIN: COPY UTIL FUNCTIONS FROM testmod_yamlcpp.cpp
#define OPEN_KCONFIG_PLUGIN()                                                                                                              \
	CppKeySet modules{ 0, KS_END };                                                                                                    \
	CppKeySet config{ 0, KS_END };                                                                                                     \
	elektraModulesInit (modules.getKeySet (), 0);                                                                                      \
	CppKey parent{ "system:/elektra/modules/kconfig", KEY_END };                                                                       \
	Plugin * plugin = elektraPluginOpen ("kconfig", modules.getKeySet (), config.getKeySet (), *parent);                               \
	exit_if_fail (plugin != NULL, "Could not open kconfig plugin")
#define CLOSE_PLUGIN()                                                                                                                     \
	elektraPluginClose (plugin, 0);                                                                                                    \
	elektraModulesClose (modules.getKeySet (), 0);                                                                                     \
	ksDel (modules.release ());                                                                                                        \
	config.release ()

static void test_files_equal (string const & filenameA, string const & filenameB)
{
	std::ifstream fileA{ filenameA };
	std::ifstream fileB{ filenameB };

	succeed_if_same (true, fileA.is_open (), "Could not open " + filenameA);
	succeed_if_same (true, fileB.is_open (), "Could not open " + filenameB);

	std::size_t counter{ 0 };
	std::string lineA;
	std::string lineB;

	while (true)
	{
		counter++;
		bool isEndOfFileA = fileA.eof ();
		bool isEndOfFileB = fileB.eof ();

		stringstream errorMessageStream;
		errorMessageStream << "Difference in line " << counter << " when comparing " << filenameA << " to " << filenameB;
		string errorMessage{ errorMessageStream.str () };

		succeed_if_same (isEndOfFileA, isEndOfFileB, errorMessage);

		if (isEndOfFileA || isEndOfFileB) break;

		getline (fileA, lineA);
		getline (fileB, lineB);

		succeed_if_same (lineA, lineB, errorMessage);
	}
}

static void read_from_kconfig_file (CppKey & loadParent, CppKeySet & loadKeySet)
{
	OPEN_KCONFIG_PLUGIN ();

	succeed_if_same (plugin->kdbGet (plugin, loadKeySet.getKeySet (), *loadParent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Call of `kdbGet` failed");

	CLOSE_PLUGIN ();
}

static void check_read_from_kconfig_file_fails (CppKey & loadParent)
{
	OPEN_KCONFIG_PLUGIN ();

	CppKeySet loadKeySet;

	succeed_if_same (plugin->kdbGet (plugin, loadKeySet.getKeySet (), *loadParent), ELEKTRA_PLUGIN_STATUS_ERROR,
			 "Call of `kdbGet` failed");

	CLOSE_PLUGIN ();
}

static void save_to_kconfig_file (CppKey & saveParent, CppKeySet & saveKeySet)
{
	OPEN_KCONFIG_PLUGIN ();

	succeed_if_same (plugin->kdbSet (plugin, saveKeySet.getKeySet (), *saveParent), ELEKTRA_PLUGIN_STATUS_SUCCESS,
			 "Call of `kdbGet` failed");

	CLOSE_PLUGIN ();
}

static void test_write (CppKey & parent, CppKeySet & expected)
{
	string filePath = elektraFilename ();

	CppKey newParent = parent.dup ();
	newParent.setString (filePath);

	save_to_kconfig_file (newParent, expected);

	test_files_equal (srcdir_file (parent.getString ().c_str ()), filePath);
}

static void test_read (CppKey & parent, CppKeySet & expected)
{
	CppKeySet result;
	CppKey loadParent = parent.dup ();
	loadParent.setString (srcdir_file (parent.getString ().c_str ()));
	read_from_kconfig_file (loadParent, result);
	compare_keyset (expected, result);
}

static void test_read_fails (std::string const & content)
{
	string filePath = elektraFilename ();
	ofstream file{ filePath };
	file << content;
	file.close ();

	CppKey loadParent;
	loadParent.setString (filePath);
	check_read_from_kconfig_file_fails (loadParent);
}

static void test_read_and_write (CppKey & parent, CppKeySet & expected)
{
	test_write (parent, expected);
	test_read (parent, expected);
}


TEST (kconfig, read_and_write_test)
{
	// Test without prefix
	{

#define TEST_VALID_PREFIX ""
		CppKeySet keys{
#include "kconfig/test_valid.h"
		};

		CppKey parent;
		parent.setString ("kconfig/test_validrc");
		test_read_and_write (parent, keys);
	}
	// Test with prefix
	{

#undef TEST_VALID_PREFIX
#define TEST_VALID_PREFIX "/kconfig/prefix"
		CppKeySet keys{
#include "kconfig/test_valid.h"
		};

		CppKey parent{ "/kconfig/prefix", KEY_END };
		parent.setString ("kconfig/test_validrc");
		test_read_and_write (parent, keys);
	}
}

TEST (kconfig, read_tests)
{
	{
		CppKeySet keys{
#include "kconfig/meta_example.h"
		};

		CppKey parent;
		parent.setString ("kconfig/meta_examplerc");
		test_read (parent, keys);
	}
	{
		CppKeySet keys{
#include "kconfig/simple_example.h"
		};

		CppKey parent;
		parent.setString ("kconfig/simple_examplerc");
		test_read (parent, keys);
	}
}


TEST (kconfig, invalid_read_tests)
{
	test_read_fails ("[openGroupName");
	test_read_fails ("multiple.locales[en][de]");
	test_read_fails ("[content after the group]name\n");
	test_read_fails ("[content after the group name] \n");
	test_read_fails ("content after[locale] textt");
	test_read_fails ("[invalid escape \\character]");
	test_read_fails ("invalid escape \\character in=key name");
	test_read_fails ("invalid escape=\\character in key value");
}

// -- Main ---------------------------------------------------------------------------------------------------------------------------------

int main (int argc, char * argv[])
{
	init (argc, argv); // Required for `srcdir_file` to work properly
	::testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}

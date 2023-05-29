/**
 * @file
 *
 * @brief Tests for ansible plugin
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "ansible.hpp"

#include <kdbmodule.h>
#include <kdbprivate.h>

using ckdb::Key;
using ckdb::KeySet;
using ckdb::Plugin;

#include <tests.h>
#include <tests.hpp>

#include <fstream>
#include <sstream>

using CppKeySet = kdb::KeySet;
using CppKey = kdb::Key;

#define PREFIX "user:/examples/ansible/"

TEST (ansible, basics) //! OCLint (avoid private static members)
#ifdef __llvm__
__attribute__ ((annotate ("oclint:suppress[empty if statement]"), annotate ("oclint:suppress[high ncss method]"),
		annotate ("oclint:suppress[too few branches in switch statement]")))
#endif
{
	CppKeySet modules{ 0, KS_END };
	CppKeySet config{ 0, KS_END };
	CppKeySet keys{ 0, KS_END };
	elektraModulesInit (modules.getKeySet (), 0);

	CppKey parent{ "system:/elektra/modules/ansible", KEY_END };
	Plugin * plugin = elektraPluginOpen ("ansible", modules.getKeySet (), config.getKeySet (), *parent);
	exit_if_fail (plugin != NULL, "Could not open ansible plugin"); //! OCLint (empty if, too few branches switch)

	succeed_if_same (plugin->kdbGet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbGet` failed");

	succeed_if_same (plugin->kdbSet (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_NO_UPDATE, "Call of `kdbSet` failed");

	succeed_if_same (plugin->kdbError (plugin, keys.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_SUCCESS, "Call of `kdbError` failed");

	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules.getKeySet (), 0);

	ksDel (modules.release ());
	config.release ();
}

string readPlaybook (const std::string & filepath)
{
	ifstream playbookFile (filepath);
	stringstream playbookStream;
	playbookStream << playbookFile.rdbuf ();
	return playbookStream.str ();
}

static void test_read_file (const string & filepath, const CppKeySet & keyset, const CppKeySet & config)
{
	CppKeySet modules{ 0, KS_END };
	CppKeySet keys{ 0, KS_END };
	elektraModulesInit (modules.getKeySet (), 0);

	CppKey parent{ "system:/elektra/modules/ansible", KEY_END };
	Plugin * plugin = elektraPluginOpen ("ansible", modules.getKeySet (), config.getKeySet (), *parent);

	auto playbook = readPlaybook (filepath);
	parent.setString (elektraFilename ());

	succeed_if_same (plugin->kdbSet (plugin, keyset.getKeySet (), *parent), ELEKTRA_PLUGIN_STATUS_NO_UPDATE,
			 parent.getMeta<string> ("error/reason"));

	auto playbookWritten = readPlaybook (parent.getString ());

	succeed_if_same (playbook, playbookWritten, "playbook should equal what has been written");


	elektraPluginClose (plugin, 0);
	elektraModulesClose (modules.getKeySet (), 0);

	ksDel (modules.release ());
}

TEST (ansible, samplePlaybook)
{
	CppKeySet config{ 0, KS_END };

	test_read_file (srcdir_file ("ansible/sample-playbook.yaml"),
#include "ansible/sample-playbook.hpp"
			, config);

	config.release ();
}

TEST (ansible, samplePlaybookWithCustomizedTexts)
{
	CppKeySet config{ 3, ckdb::keyNew ("system:/playbook/name", KEY_VALUE, "Customized Playbook Name", KEY_END),
			  ckdb::keyNew ("system:/playbook/hosts", KEY_VALUE, "customized_hosts", KEY_END),
			  ckdb::keyNew ("system:/task/main/name", KEY_VALUE, "Customized Task Name", KEY_END), KS_END };

	test_read_file (srcdir_file ("ansible/sample-playbook-customized-texts.yaml"),
#include "ansible/sample-playbook.hpp"
			, config);

	config.release ();
}

TEST (ansible, samplePlaybookOnlyTask)
{
	CppKeySet config{ 3, ckdb::keyNew ("system:/playbook", KEY_VALUE, "0", KEY_END), KS_END };

	test_read_file (srcdir_file ("ansible/sample-playbook-only-task.yaml"),
#include "ansible/sample-playbook.hpp"
			, config);

	config.release ();
}

int main (int argc, char * argv[])
{
	init (argc, argv); // Required for `srcdir_file` to work properly
	::testing::InitGoogleTest (&argc, argv);
	return RUN_ALL_TESTS ();
}

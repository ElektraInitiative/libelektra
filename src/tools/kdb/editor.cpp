#include <editor.hpp>

#include <kdb.hpp>
#include <modules.hpp>
#include <cmdline.hpp>
#include <external.hpp>
#include <keysetio.hpp>

#include <unistd.h>

#include <export.hpp>
#include <import.hpp>

#include <iostream>
#include <string>

#include <merging/threewaymerge.hpp>
#include <merging/metamergestrategy.hpp>
#include <mergehelper.hpp>

using namespace kdb;
using namespace kdb::tools;
using namespace kdb::tools::merging;
using namespace std;

EditorCommand::EditorCommand()
{
}

EditorCommand::~EditorCommand()
{
}

void EditorCommand::tmpFile()
{
	const char * tmpvar = getenv ("TMPDIR");
	if (!tmpvar)
	{
		tmpvar = "/tmp";
	}
	filename = tmpvar;
	filename += "/elektra-test.XXXXXX";
	char * fn = static_cast<char*>(malloc(filename.length()+1));
	strcpy(fn, filename.c_str());
	fd = mkstemp (fn);
	filename = std::string(fn);
	close(fd);
	free (fn);
}

int EditorCommand::execute(Cmdline const& cl)
{
	int argc = cl.arguments.size ();
	if (argc < 1)
	{
		throw invalid_argument ("wrong number of arguments, 1 needed");
	}

	Key root (cl.arguments[0], KEY_END);

	KeySet ours;
	KDB kdb;
	kdb.get (ours, root);
	KeySet oursToEdit = ours.cut (root);

	// export it to file
	string format = cl.format;
	if (argc > 1) format = cl.arguments[1];

	Modules modules;
	PluginPtr plugin = modules.load(format);

	tmpFile();
	if (cl.verbose) std::cout << "filename set to " << filename << std::endl;
	Key errorKey(root);
	errorKey.setString(filename);

	plugin->set(oursToEdit, errorKey);

	printWarnings(cerr, errorKey);
	printError(cerr, errorKey);


	// start editor
	if (cl.verbose) std::cout << "running editor with " << filename << std::endl;
	runEditor(filename);

	// import from the file
	KeySet importedKeys;
	plugin->get(importedKeys, errorKey);
	importedKeys = importedKeys.cut(root);

	printWarnings (cerr, errorKey);
	printError (cerr, errorKey);

	ThreeWayMerge merger;
	MergeHelper helper;

	helper.configureMerger (cl, merger);
	MergeResult result = merger.mergeKeySet (
			MergeTask (BaseMergeKeys (oursToEdit, root), OurMergeKeys (oursToEdit, root),
					TheirMergeKeys (importedKeys, root), root));

	helper.reportResult (cl, result, cout, cerr);

	int ret = -1;
	if (!result.hasConflicts ())
	{
		if (cl.verbose)
		{
			cout << "The merged keyset with strategy " << cl.strategy << " is:" << endl;
			cout << result.getMergedKeys();
		}

		KeySet resultKeys = result.getMergedKeys();
		if (cl.verbose) std::cout << "about to write result keys " << resultKeys << std::endl;
		ours.append(resultKeys);
		kdb.set (ours, root);
		ret = 0;
		if (cl.verbose) std::cout << "successful, cleaning up " << filename << std::endl;
		unlink(filename.c_str());
	}
	else
	{
		std::cout << "Import not successful, please import and remove \"" << filename << '"' << std::endl;
	}

	return ret;
}


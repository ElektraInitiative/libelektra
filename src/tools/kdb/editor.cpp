#include <editor.hpp>

#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

#include <cmdline.hpp>
#include <external.hpp>
#include <kdb.hpp>
#include <keysetio.hpp>
#include <modules.hpp>
#include <plugindatabase.hpp>

#include <elektra/kdbmacros.h>

#include <export.hpp>
#include <import.hpp>

#include <iostream>
#include <string>

#include <mergehelper.hpp>
#include <merging/metamergestrategy.hpp>
#include <merging/threewaymerge.hpp>

using namespace kdb;
using namespace kdb::tools;
using namespace kdb::tools::merging;
using namespace std;

EditorCommand::EditorCommand ()
{
}

EditorCommand::~EditorCommand ()
{
}

void EditorCommand::tmpFile ()
{
#ifndef _WIN32
	const char * tmpvar = getenv ("TMPDIR");
	if (!tmpvar)
	{
		tmpvar = "/tmp";
	}
	filename = tmpvar;
	filename += "/elektra-test.XXXXXX";
	char * fn = static_cast<char *> (malloc (filename.length () + 1));
	strcpy (fn, filename.c_str ());
	int fd = mkstemp (fn);
	filename = std::string (fn);
	close (fd);
	free (fn);
#endif
}

bool runAllEditors (std::string const & filename)
{
	using namespace kdb;
	if (runEditor ("/usr/bin/sensible-editor", filename)) return true;
	if (runEditor ("/usr/bin/editor", filename)) return true;
	char * editor = getenv ("EDITOR");
	if (editor && runEditor (editor, filename)) return true;
	if (runEditor ("/usr/bin/vi", filename)) return true;
	if (runEditor ("/bin/vi", filename)) return true;
	return false;
}

class EditorNotAvailable : public std::exception
{
	virtual const char * what () const throw () override
	{
		return "kdb-editor not available for windows (non-POSIX systems)";
	}
};

int EditorCommand::execute (Cmdline const & cl)
{
#ifdef _WIN32
	throw EditorNotAvailable ();
#endif

	int argc = cl.arguments.size ();
	if (argc < 1)
	{
		throw invalid_argument ("wrong number of arguments, 1 needed");
	}
	Key root = cl.createKey (0, false);

	KeySet ours;
	KDB kdb;
	kdb.get (ours, root);
	KeySet oursToEdit = ours.cut (root);
	KeySet original = oursToEdit.dup ();

	// export it to file
	string format = cl.format;
	if (argc > 1) format = cl.arguments[1];

	if (cl.verbose) std::cout << "lookup provider for: " << format << endl;

	ModulesPluginDatabase pluginDatabase;
	PluginSpec provides = pluginDatabase.lookupProvides (format);

	if (cl.verbose) std::cout << "found provider: " << provides.getName () << endl;

	Modules modules;
	PluginPtr plugin = modules.load (provides.getName ());

	tmpFile ();
	if (cl.verbose) std::cout << "filename set to " << filename << std::endl;
	Key errorKey (root);
	errorKey.setString (filename);
	struct stat orig;
	stat (filename.c_str (), &orig);

	if (plugin->set (oursToEdit, errorKey) == -1)
	{
		printWarnings (cerr, errorKey, cl.verbose, cl.debug);
		printError (cerr, errorKey, cl.verbose, cl.debug);
		return 11;
	}

	printWarnings (cerr, errorKey, cl.verbose, cl.debug);


	// start editor
	if (cl.verbose) std::cout << "running editor with " << filename << std::endl;
	if (!cl.editor.empty ())
	{
		if (!runEditor (cl.editor, filename))
		{
			std::cerr << "Could not run editor " << cl.editor << std::endl;
			return 12;
		}
	}
	else
	{
		if (!runAllEditors (filename))
		{
			std::cerr << "Could not run any editor, please change /sw/elektra/kdb/#0/current/editor" << std::endl;
			return 12;
		}
	}

	struct stat modif;
	stat (filename.c_str (), &modif);

	if (ELEKTRA_STAT_SECONDS (orig) == ELEKTRA_STAT_SECONDS (modif) &&
	    ELEKTRA_STAT_NANO_SECONDS (orig) == ELEKTRA_STAT_NANO_SECONDS (modif))
	{
		if (!cl.quiet) std::cout << "File was unchanged, will exit successfully";
		return 0;
	}

	// import from the file
	KeySet importedKeys;
	plugin->get (importedKeys, errorKey);
	importedKeys = importedKeys.cut (root);

	printWarnings (cerr, errorKey, cl.verbose, cl.debug);
	printError (cerr, errorKey, cl.verbose, cl.debug);

	ThreeWayMerge merger;
	MergeHelper helper;

	helper.configureMerger (cl, merger);
	MergeResult result = merger.mergeKeySet (
		MergeTask (BaseMergeKeys (oursToEdit, root), OurMergeKeys (oursToEdit, root), TheirMergeKeys (importedKeys, root), root));

	helper.reportResult (cl, result, cout, cerr);

	int ret = 13;
	if (!result.hasConflicts ())
	{
		if (cl.verbose)
		{
			cout << "The merged keyset with strategy " << cl.strategy << " is:" << endl;
			cout << result.getMergedKeys ();
		}

		KeySet resultKeys = result.getMergedKeys ();
		if (cl.verbose) std::cout << "about to write result keys " << resultKeys << std::endl;
		ours.append (resultKeys);
		try
		{
			kdb.set (ours, root);
			if (cl.verbose) std::cout << "successful, cleaning up " << filename << std::endl;
			unlink (filename.c_str ());
			ret = 0;
		}
		catch (KDBException const & e)
		{
			std::cout << "Import of configuration failed with the error:\n";
			std::cout << e.what ();
			std::cout << "\n\n";
			std::cout << "Your changes are not lost." << std::endl;
			std::cout << "Please fix, import and remove \"" << filename << '"' << std::endl;
			ret = 14;
		}
	}
	else
	{
		std::cout << "Import not successful, please import and remove \"" << filename << '"' << std::endl;
	}

	return ret;
}

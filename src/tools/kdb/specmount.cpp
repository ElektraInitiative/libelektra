/**
 * @file
 *
 * @brief source file of spec mount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#include <cmdline.hpp>
#include <specmount.hpp>
#include <specreader.hpp>

#include <fstream>
#include <iostream>
#include <iterator>
#include <string>
#include <vector>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

SpecMountCommand::SpecMountCommand ()
{
}

void SpecMountCommand::setMountpoint (Cmdline const & cl)
{
	if (cl.arguments.empty ())
	{
		throw invalid_argument ("you need to provide one argument: spec-mountpoint");
	}

	mp = cl.createKey (0).getName ();

	if (mp.at (0) != '/')
	{
		throw invalid_argument (mp + " is not a cascading mountpoint");
	}
}

void SpecMountCommand::buildBackend (Cmdline const & cl)
{
	SpecReader sr;

	kdb::KeySet specToRead;
	kdb.get (specToRead, "spec:" + mp);
	specToRead = specToRead.cut (Key ("spec:" + mp, KEY_END));

	sr.readSpecification (specToRead);

	SpecReader::Backends const & backends = sr.getBackends ();

	for (auto & p : backends)
	{
		auto backend = p.second;
		if (cl.verbose)
		{
			std::cout << "Got mountpoint from " << p.first.getName () << " with " << backend.nodes
				  << " nodes, configfile: " << backend.getConfigFile () << " and mountpoint: " << backend.getMountpoint ()
				  << std::endl;
		}

		backend.setBackendConfig (cl.getPluginsConfig ("system:/"));
		backend.needPlugin (cl.resolver);
		backend.needPlugin ("storage");

		backend.addPlugins (parseArguments (cl.plugins));

		const int alreadyRead = 1; // we already read mountpoint
		if (cl.arguments.size () <= alreadyRead)
		{
			backend.addPlugins (parseArguments (cl.arguments.begin () + alreadyRead, cl.arguments.end ()));
		}

		// Call it a day
		outputMissingRecommends (backend.resolveNeeds (cl.withRecommends));
		Backends::umount (backend.getMountpoint (), mountConf);
		backend.serialize (mountConf);
	}

	if (!cl.quiet && backends.empty ())
	{
		std::cout << "No metadata \"mountpoint\" found on key \"spec:" << mp << "\"" << std::endl;
	}
}

/**
 * @brief Its quite linear whats going on, but there are many steps involved
 *
 * @param cl the commandline
 *
 * @retval 0 on success (otherwise exception)
 */
int SpecMountCommand::execute (Cmdline const & cl)
{
	readMountConf (cl);

	setMountpoint (cl);

	buildBackend (cl);
	doIt ();

	return 0;
}

SpecMountCommand::~SpecMountCommand ()
{
}

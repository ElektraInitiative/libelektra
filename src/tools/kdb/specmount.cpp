/**
 * @file
 *
 * @brief source file of spec mount command
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#include <specmount.hpp>
#include <cmdline.hpp>
#include <specreader.hpp>

#include <iostream>
#include <iterator>
#include <fstream>
#include <vector>
#include <string>

using namespace std;
using namespace kdb;
using namespace kdb::tools;

SpecMountCommand::SpecMountCommand()
{}

void SpecMountCommand::setMountpoint (Cmdline const& cl)
{
	if (cl.arguments.empty())
	{
		throw invalid_argument("you need to provide one argument: spec-mountpoint");
	}

	mp = cl.createKey(0).getName();
	Key mpk(mp, KEY_CASCADING_NAME, KEY_END);

	if (!mpk.isValid())
	{
		throw invalid_argument(mp + " is not a valid mountpoint");
	}

	if (mp.at(0) != '/')
	{
		throw invalid_argument(mp + " is not a cascading mountpoint");
	}
}

void SpecMountCommand::buildBackend (Cmdline const& cl)
{
	SpecReader sr;

	kdb::KeySet specToRead;
	kdb.get(specToRead, "spec"+mp);

	sr.readSpecification(specToRead);

	SpecReader::Backends const & backends = sr.getBackends();

	for (auto & p : backends)
	{
		auto backend = p.second;
		// TODO use p.first; // relative mountpoint

		backend.needPlugin (cl.resolver);
		backend.needPlugin ("storage");

		backend.addPlugins (parseArguments (cl.plugins));

		const int alreadyRead = 1; // we already read mountpoint
		if (cl.arguments.size() <= alreadyRead)
		{
			backend.addPlugins (parseArguments (cl.arguments.begin()+alreadyRead, cl.arguments.end()));
		}

		// Call it a day
		outputMissingRecommends(backend.resolveNeeds(cl.withRecommends));
		backend.serialize (mountConf);
	}
}

/**
 * @brief Its quite linear whats going on, but there are many steps involved
 *
 * @param cl the commandline
 *
 * @retval 0 on success (otherwise exception)
 */
int SpecMountCommand::execute (Cmdline const& cl)
{
	readMountConf(cl);

	setMountpoint(cl);

	buildBackend(cl);
	doIt();

	return 0;
}

SpecMountCommand::~SpecMountCommand()
{}

/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef MOUNTBASE_HPP_
#define MOUNTBASE_HPP_

#include "./coloredkdbio.hpp"
#include <command.hpp>
#include <kdb.hpp>

#include <backends.hpp>

#include <vector>

class MountBaseCommand : public Command
{

protected:
	void readMountConf (Cmdline const & cl);
	void getMountpoint (Cmdline const & cl);
	void askForConfirmation (Cmdline const & cl);
	void outputMissingRecommends (std::vector<std::string> missingRecommends);
	void doIt ();

	std::string mountpointsPath = kdb::tools::Backends::mountpointsPath;
	kdb::KDB kdb;
	kdb::KeySet mountConf;
	std::string path;
	std::string mp;

public:
};


#endif /* MOUNTBASE_HPP_ */

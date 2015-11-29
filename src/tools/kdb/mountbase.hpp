/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef MOUNTBASE_HPP_
#define MOUNTBASE_HPP_

#include <command.hpp>
#include <kdb.hpp>

namespace kdb
{
namespace tools
{
	class Backend;
}
}

class MountBaseCommand : public Command
{

protected:
	void readMountConf(Cmdline const& cl);
	void getMountpoint(Cmdline const& cl);
	void askForConfirmation(Cmdline const& cl);
	void doIt();

	kdb::KDB kdb;
	kdb::KeySet mountConf;
	std::string path;
	std::string mp;

public:
};



#endif /* MOUNTBASE_HPP_ */

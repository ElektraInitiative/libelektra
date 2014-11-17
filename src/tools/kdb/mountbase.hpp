
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
	void fixRootKey(Cmdline const& cl);
	void getName(Cmdline const& cl);
	void getMountpoint(Cmdline const& cl);
	void askForConfirmation(Cmdline const& cl);
	void doIt();

	kdb::KeySet mountConf;
	std::string name;
	std::string path;
	std::string mp;

public:
};



#endif /* MOUNTBASE_HPP_ */

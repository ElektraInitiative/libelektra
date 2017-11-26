/**
 * @file
 *
 * @brief header file of spec mount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef GLOBAL_MOUNT_HPP
#define GLOBAL_MOUNT_HPP

#include <mountbase.hpp>

namespace kdb
{
namespace tools
{
class MountBackendInterface;
}
}

class GlobalMountCommand : public MountBaseCommand
{
	void outputMtab (Cmdline const & cl);
	std::vector<std::string> getMtab ();
	void setMountpoint (Cmdline const & cl);
	void buildBackend (Cmdline const & cl);

public:
	GlobalMountCommand ();
	~GlobalMountCommand ();

	virtual std::string getShortOptions () override
	{
		return "W"; // TODO: c not implemented
	}

	virtual std::string getSynopsis () override
	{
		return "[plugin [config] [..]]";
	}

	virtual std::string getShortHelpText () override
	{
		return "Globally mount given plugins.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif

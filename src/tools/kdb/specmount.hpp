/**
 * @file
 *
 * @brief header file of spec mount command
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */


#ifndef SPEC_MOUNT_HPP
#define SPEC_MOUNT_HPP

#include <mountbase.hpp>

namespace kdb
{
namespace tools
{
class MountBackendInterface;
}
} // namespace kdb

class SpecMountCommand : public MountBaseCommand
{
	void outputMtab (Cmdline const & cl);
	void setMountpoint (Cmdline const & cl);
	void buildBackend (Cmdline const & cl);

public:
	SpecMountCommand ();
	~SpecMountCommand ();

	virtual std::string getShortOptions () override
	{
		return "qRcW";
	}

	virtual std::string getSynopsis () override
	{
		return "[mountpoint] [plugin [config] [..]]";
	}

	virtual std::string getShortHelpText () override
	{
		return "Mount a new backend by specification. All necessary plugins will be loaded automatically, depending on the "
		       "metadata.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif

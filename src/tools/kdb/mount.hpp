/**
 * @file
 *
 * @brief header file of mount command
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 *
 */


#ifndef MOUNT_HPP
#define MOUNT_HPP

#include <mountbase.hpp>

namespace kdb
{
namespace tools
{
class MountBackendInterface;
}
}

class MountCommand : public MountBaseCommand
{
	void outputMtab (Cmdline const & cl);
	void processArguments (Cmdline const & cl);
	void buildBackend (Cmdline const & cl);
	void appendPlugins (kdb::tools::MountBackendInterface & backend);
	void readPluginConfig (kdb::KeySet & config);

public:
	MountCommand ();
	~MountCommand ();

	virtual std::string getShortOptions () override
	{
		return "idR0123cWC";
	}

	virtual std::string getSynopsis () override
	{
		return "[path mountpoint] [plugin [config] [..]]";
	}

	virtual std::string getShortHelpText () override
	{
		return "Mount a new backend.";
	}

	virtual std::string getLongHelpText () override
	{
		return "path .. a filename (absolute for system, relative for cascading or user)\n"
		       "mountpoint .. where to mount the backend, start with / for cascading mp\n"
		       "plugin .. a list of plugins and their config to mount at that place\n"
		       "  Each plugin my be followed by a (,-sep) list of keys and corresponding values that will be\n"
		       "  written below the backend config. For example param1=value1,param2=value2\n"
		       "\n"
		       "With the -i option, the mounting will be done interactively\n"
		       "\n"
		       "With no arguments and not in interactive mode, the current mountpoints will be listed\n"
		       "Then the options -0123 take effect (otherwise these options can be used to suppress warnings).\n"
		       "1,2 and 3 will suppress the output of the respective column\n"
		       "\n"
		       "\n"
		       "Examples:\n"
		       "\n"
		       "Null terminated output of path and backend name:\n"
		       "kdb mount -02 | xargs -0n 2 echo\n"
		       "\n"
		       "Mount /etc/file to system file with two plugins and a respective configuration option each:\n"
		       "kdb mount /etc/file system/file plugin1 plugin1config=config1 plugin2 plugin2config=config2\n"
		       "\n"
		       "recode and rename\n"
		       "kdb mount s.ini recode.txt ni rename cut=path iconv recode=utf8..latin1\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif

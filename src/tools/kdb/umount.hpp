#ifndef UMOUNT_HPP
#define UMOUNT_HPP

#include <command.hpp>

#include <kdb.hpp>

class UmountCommand : public Command
{
	kdb::KDB kdb;

public:
	UmountCommand();
	~UmountCommand();

	virtual std::string getShortOptions()
	{
		return "";
	}

	virtual std::string getSynopsis()
	{
		return "<name>";
	}

	virtual std::string getShortHelpText()
	{
		return "Unmount backend from key database.";
	}

	virtual std::string getLongHelpText()
	{
		return "";
	}

	virtual int execute (Cmdline const& cmdline);

private:
	int deleteByBackendName(kdb::KeySet& mountConf, std::string const & backendName);
	int deleteByMountPath(kdb::KeySet& mountCOnf, std::string const & mountPath);
};

#endif

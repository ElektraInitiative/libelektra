/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef REMOUNT_HPP_
#define REMOUNT_HPP_

#include <mountbase.hpp>

class RemountCommand : public MountBaseCommand
{
	void getExistingMountpoint(Cmdline const & cl);
	void cloneMountpoint(Cmdline const & cl);

	std::string existingName;

public:
	RemountCommand();
	~RemountCommand();

	virtual std::string getShortOptions()
	{
		return "id";
	}

	virtual std::string getSynopsis()
	{
		return "<new filename> <new path> <existing mountpoint>";
	}

	virtual std::string getShortHelpText()
	{
		return "Remount an existing backend with a different filename.";
	}

	virtual std::string getLongHelpText()
	{
		return "";
	}

	virtual int execute (Cmdline const& cmdline);
};



#endif /* REMOUNT_HPP_ */

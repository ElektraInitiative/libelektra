/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#ifndef REMOUNT_HPP_
#define REMOUNT_HPP_

#include <mountbase.hpp>

class RemountCommand : public MountBaseCommand
{
	void getExistingMountpoint (Cmdline const & cl);
	void cloneMountpoint (Cmdline const & cl);

	std::string existingName;

public:
	RemountCommand ();
	~RemountCommand ();

	virtual std::string getShortOptions () override
	{
		return "idC";
	}

	virtual std::string getSynopsis () override
	{
		return "<new filename> <new path> <existing mountpoint>";
	}

	virtual std::string getShortHelpText () override
	{
		return "Remount an existing backend with a different filename.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};


#endif /* REMOUNT_HPP_ */

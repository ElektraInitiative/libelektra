/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CACHE_HPP
#define CACHE_HPP

#include "./coloredkdbio.hpp"
#include <command.hpp>

class CacheCommand : public Command
{
public:
	CacheCommand ();
	~CacheCommand ();

	virtual std::string getShortOptions () override
	{
		return "";
	}

	virtual std::string getSynopsis () override
	{
		return "{enable,disable,default,clear}";
	}

	virtual std::string getShortHelpText () override
	{
		return "Enable, disable, clear the cache or revert to default.";
	}

	virtual std::string getLongHelpText () override
	{
		return "This command is used to enable or disable the cache and to revert\n"
		       "to the default settings. The default settings will let the system\n"
		       "decide whether to use the cache or not. The clear command will\n"
		       "remove the generated cache files in a safe way.\n";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif

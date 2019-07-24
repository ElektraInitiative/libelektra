/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef CACHE_HPP
#define CACHE_HPP

#include "coloredkdbio.hpp"
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
		return "{enable,disable,clear}";
	}

	virtual std::string getShortHelpText () override
	{
		return "Enable, disable or clear the cache.";
	}

	virtual std::string getLongHelpText () override
	{
		return "";
	}

	virtual int execute (Cmdline const & cmdline) override;
};

#endif

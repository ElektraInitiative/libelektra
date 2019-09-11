/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_COLOREDKDB_IO_HPP
#define ELEKTRA_COLOREDKDB_IO_HPP

/*
 * @brief See examples/cpp_example_userio.cpp for how to use
 * USER_DEFINED_IO
 */
#define USER_DEFINED_IO

/* Do not include `keyio.hpp` and `keysetio.hpp` */
#ifndef ELEKTRA_KDB_IO_HPP
#define ELEKTRA_KDB_IO_HPP

#include "ansicolors.hpp"

#include <key.hpp>

#include <iomanip>
#include <ostream>

namespace kdb
{

inline std::ostream & printError (std::ostream & os, kdb::Key const & error, bool printVerbose, bool printDebug)
{
	try
	{
		if (!error.getMeta<const kdb::Key> ("error"))
		{
			// no error available
			return os;
		}
		os << "Sorry, module " << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::BLUE)
		   << error.getMeta<std::string> ("error/module") << getErrorColor (ANSI_COLOR::RESET) << " issued the error "
		   << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::RED) << error.getMeta<std::string> ("error/number")
		   << getErrorColor (ANSI_COLOR::RESET) << ":" << std::endl;
		os << error.getMeta<std::string> ("error/description") << ": " << error.getMeta<std::string> ("error/reason") << std::endl;

		if (printVerbose)
		{
			os << getErrorColor (ANSI_COLOR::BOLD) << "Mountpoint: " << getErrorColor (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> ("error/mountpoint") << std::endl;
			os << getErrorColor (ANSI_COLOR::BOLD) << "Configfile: " << getErrorColor (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> ("error/configfile") << std::endl;
		}

		if (printDebug)
		{
			os << getErrorColor (ANSI_COLOR::BOLD) << "At: " << getErrorColor (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> ("error/file") << ":" << error.getMeta<std::string> ("error/line") << std::endl;
		}
	}
	catch (kdb::KeyTypeConversion const & e)
	{
		os << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::RED)
		   << "Sorry, error metadata is not set correctly by a plugin: " << getErrorColor (ANSI_COLOR::RESET) << e.what ()
		   << std::endl;
	}

	return os;
}

inline std::ostream & printWarnings (std::ostream & os, kdb::Key const & error, bool printVerbose, bool printDebug)
{
	try
	{
		if (!error.getMeta<const kdb::Key> ("warnings"))
		{
			// no warnings were issued
			return os;
		}

		int nr = error.getMeta<int> ("warnings");
		os << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::MAGENTA) << " Sorry, " << nr + 1 << " warning"
		   << (!nr ? " was" : "s were") << " issued ;(" << getErrorColor (ANSI_COLOR::RESET) << std::endl;

		for (int i = 0; i <= nr; i++)
		{
			std::ostringstream name;
			name << "warnings/#" << std::setfill ('0') << std::setw (2) << i;
			// os << "\t" << name.str() << ": " << error.getMeta<std::string>(name.str()) << std::endl;
			os << "\tSorry, module " << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::BLUE)
			   << error.getMeta<std::string> (name.str () + "/module") << getErrorColor (ANSI_COLOR::RESET)
			   << " issued the warning " << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::RED)
			   << error.getMeta<std::string> (name.str () + "/number") << getErrorColor (ANSI_COLOR::RESET) << ":" << std::endl;
			os << "\t" << error.getMeta<std::string> (name.str () + "/description") << ": "
			   << error.getMeta<std::string> (name.str () + "/reason") << std::endl;
			if (printVerbose)
			{
				os << getErrorColor (ANSI_COLOR::BOLD) << "\tMountpoint: " << getErrorColor (ANSI_COLOR::RESET)
				   << error.getMeta<std::string> (name.str () + "/mountpoint") << std::endl;
				os << getErrorColor (ANSI_COLOR::BOLD) << "\tConfigfile: " << getErrorColor (ANSI_COLOR::RESET)
				   << error.getMeta<std::string> (name.str () + "/configfile") << std::endl;
			}
			if (printDebug)
			{
				os << getErrorColor (ANSI_COLOR::BOLD) << "\tAt: " << getErrorColor (ANSI_COLOR::RESET)
				   << error.getMeta<std::string> (name.str () + "/file") << ":"
				   << error.getMeta<std::string> (name.str () + "/line") << std::endl;
			}
		}
	}
	catch (kdb::KeyTypeConversion const & e)
	{
		os << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::MAGENTA)
		   << "Sorry, warnings metadata not set correctly by a plugin: " << getErrorColor (ANSI_COLOR::RESET) << e.what ()
		   << std::endl;
	}

	return os;
}
} // namespace kdb

#endif
#endif

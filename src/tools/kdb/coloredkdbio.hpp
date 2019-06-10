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

inline std::ostream & printError (std::ostream & os, kdb::Key const & error)
{
	try
	{
		if (!error.getMeta<const kdb::Key> ("error"))
		{
			// no error available
			return os;
		}

		os << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::RED) << "Sorry, the error (#"
		   << error.getMeta<std::string> ("error/number") << ") occurred ;(" << getErrorColor (ANSI_COLOR::RESET) << std::endl;
		os << getErrorColor (ANSI_COLOR::BOLD) << "Description: " << getErrorColor (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/description") << std::endl;
		os << getErrorColor (ANSI_COLOR::BOLD) << "Reason: " << getErrorColor (ANSI_COLOR::YELLOW)
		   << error.getMeta<std::string> ("error/reason") << getErrorColor (ANSI_COLOR::RESET) << std::endl;
		os << getErrorColor (ANSI_COLOR::BOLD) << "Module: " << getErrorColor (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/module") << std::endl;
		os << getErrorColor (ANSI_COLOR::BOLD) << "At: " << getErrorColor (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/file") << ":" << error.getMeta<std::string> ("error/line") << std::endl;
		os << getErrorColor (ANSI_COLOR::BOLD) << "Mountpoint: " << getErrorColor (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/mountpoint") << std::endl;
		os << getErrorColor (ANSI_COLOR::BOLD) << "Configfile: " << getErrorColor (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/configfile") << std::endl;
	}
	catch (kdb::KeyTypeConversion const & e)
	{
		os << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::RED)
		   << "Sorry, error metadata is not set correctly by a plugin: " << getErrorColor (ANSI_COLOR::RESET) << e.what ()
		   << std::endl;
	}

	return os;
}

inline std::ostream & printWarnings (std::ostream & os, kdb::Key const & error)
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
			os << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::MAGENTA) << " Warning "
			   << "(#" << error.getMeta<std::string> (name.str () + "/number") << "):" << getErrorColor (ANSI_COLOR::RESET)
			   << std::endl;
			os << getErrorColor (ANSI_COLOR::BOLD) << "\tDescription: " << getErrorColor (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/description") << std::endl;
			os << getErrorColor (ANSI_COLOR::BOLD) << "\tModule: " << getErrorColor (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/module") << std::endl;
			os << getErrorColor (ANSI_COLOR::BOLD) << "\tAt: " << getErrorColor (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/file") << ":"
			   << error.getMeta<std::string> (name.str () + "/line") << std::endl;
			os << getErrorColor (ANSI_COLOR::BOLD) << "\tReason: " << getErrorColor (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/reason") << std::endl;
			os << getErrorColor (ANSI_COLOR::BOLD) << "\tMountpoint: " << getErrorColor (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/mountpoint") << std::endl;
			os << getErrorColor (ANSI_COLOR::BOLD) << "\tConfigfile: " << getErrorColor (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/configfile") << std::endl;
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

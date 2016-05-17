/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_IO_HPP
#define ELEKTRA_KDB_IO_HPP

/*
 * @brief See examples/cpp_example_userio.cpp for how to use
 * USER_DEFINED_IO
 */
#define USER_DEFINED_IO

#include <key.hpp>

#include <iomanip>
#include <ostream>

#include "ansicolors.hpp"

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

		os << getColorEscape (ANSI_COLOR::BOLD) << getColorEscape (ANSI_COLOR::RED) << "Error (#"
		   << error.getMeta<std::string> ("error/number") << ") occurred!" << getColorEscape (ANSI_COLOR::RESET) << std::endl;
		os << getColorEscape (ANSI_COLOR::BOLD) << "Description: " << getColorEscape (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/description") << std::endl;
		os << getColorEscape (ANSI_COLOR::BOLD) << "Ingroup: " << getColorEscape (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/ingroup") << std::endl;
		os << getColorEscape (ANSI_COLOR::BOLD) << "Module: " << getColorEscape (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/module") << std::endl;
		os << getColorEscape (ANSI_COLOR::BOLD) << "At: " << getColorEscape (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/file") << ":" << error.getMeta<std::string> ("error/line") << std::endl;
		os << getColorEscape (ANSI_COLOR::BOLD) << "Reason: " << getColorEscape (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/reason") << std::endl;
		os << getColorEscape (ANSI_COLOR::BOLD) << "Mountpoint: " << getColorEscape (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/mountpoint") << std::endl;
		os << getColorEscape (ANSI_COLOR::BOLD) << "Configfile: " << getColorEscape (ANSI_COLOR::RESET)
		   << error.getMeta<std::string> ("error/configfile") << std::endl;
	}
	catch (kdb::KeyTypeConversion const & e)
	{
		os << getColorEscape (ANSI_COLOR::BOLD) << getColorEscape (ANSI_COLOR::RED)
		   << "Error meta data is not set correctly by a plugin: " << getColorEscape (ANSI_COLOR::RESET) << e.what () << std::endl;
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
		os << getColorEscape (ANSI_COLOR::BOLD) << getColorEscape (ANSI_COLOR::MAGENTA) << nr + 1 << " Warning" << (!nr ? "" : "s")
		   << " were issued:" << getColorEscape (ANSI_COLOR::RESET) << std::endl;

		for (int i = 0; i <= nr; i++)
		{
			std::ostringstream name;
			name << "warnings/#" << std::setfill ('0') << std::setw (2) << i;
			// os << "\t" << name.str() << ": " << error.getMeta<std::string>(name.str()) << std::endl;
			os << getColorEscape (ANSI_COLOR::BOLD) << getColorEscape (ANSI_COLOR::MAGENTA)
			   << " Warning number: " << getColorEscape (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/number") << std::endl;
			os << getColorEscape (ANSI_COLOR::BOLD) << "\tDescription: " << getColorEscape (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/description") << std::endl;
			os << getColorEscape (ANSI_COLOR::BOLD) << "\tIngroup: " << getColorEscape (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/ingroup") << std::endl;
			os << getColorEscape (ANSI_COLOR::BOLD) << "\tModule: " << getColorEscape (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/module") << std::endl;
			os << getColorEscape (ANSI_COLOR::BOLD) << "\tAt: " << getColorEscape (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/file") << ":"
			   << error.getMeta<std::string> (name.str () + "/line") << std::endl;
			os << getColorEscape (ANSI_COLOR::BOLD) << "\tReason: " << getColorEscape (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/reason") << std::endl;
			os << getColorEscape (ANSI_COLOR::BOLD) << "\tMountpoint: " << getColorEscape (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/mountpoint") << std::endl;
			os << getColorEscape (ANSI_COLOR::BOLD) << "\tConfigfile: " << getColorEscape (ANSI_COLOR::RESET)
			   << error.getMeta<std::string> (name.str () + "/configfile") << std::endl;
		}
	}
	catch (kdb::KeyTypeConversion const & e)
	{
		os << getColorEscape (ANSI_COLOR::BOLD) << getColorEscape (ANSI_COLOR::MAGENTA)
		   << "Warnings meta data not set correctly by a plugin: " << getColorEscape (ANSI_COLOR::RESET) << e.what () << std::endl;
	}

	return os;
}
}

#endif

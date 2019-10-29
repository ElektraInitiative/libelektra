/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef ELEKTRA_KDB_IO_HPP
#define ELEKTRA_KDB_IO_HPP

/*
 * @brief See examples/cpp_example_userio.cpp for how to use
 * USER_DEFINED_IO
 */
#ifndef USER_DEFINED_IO

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

		os << "Sorry, module " << error.getMeta<std::string> ("error/module") << " issued the error "
		   << error.getMeta<std::string> ("error/number") << ":" << std::endl;
		os << error.getMeta<std::string> ("error/description") << ": " << error.getMeta<std::string> ("error/reason") << std::endl;
		if (printVerbose)
		{
			os << "Mountpoint: " << error.getMeta<std::string> ("error/mountpoint") << std::endl;
			os << "Configfile: " << error.getMeta<std::string> ("error/configfile") << std::endl;
		}
		if (printDebug)
		{
			os << "At: " << error.getMeta<std::string> ("error/file") << ":" << error.getMeta<std::string> ("error/line")
			   << std::endl;
		}
	}
	catch (kdb::KeyTypeConversion const & e)
	{
		os << "Error metadata is not set correctly by a plugin: " << e.what () << std::endl;
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
		if (!nr)
		{
			os << "1 Warning was issued:" << std::endl;
		}
		else
		{
			os << nr + 1 << " Warnings were issued:" << std::endl;
		}

		for (int i = 0; i <= nr; i++)
		{
			std::ostringstream name;
			name << "warnings/#" << i;
			os << "\tSorry, module " << error.getMeta<std::string> (name.str () + "/module") << " issued the warning "
			   << error.getMeta<std::string> (name.str () + "/number") << ":" << std::endl;
			os << "\t" << error.getMeta<std::string> (name.str () + "/description") << ": "
			   << error.getMeta<std::string> (name.str () + "/reason") << std::endl;
			// os << "\t" << name.str() << ": " << error.getMeta<std::string>(name.str()) << std::endl;
			if (printVerbose)
			{
				os << "\tMountpoint: " << error.getMeta<std::string> (name.str () + "/mountpoint") << std::endl;
				os << "\tConfigfile: " << error.getMeta<std::string> (name.str () + "/configfile") << std::endl;
			}
			if (printDebug)
			{
				os << "\tAt: " << error.getMeta<std::string> (name.str () + "/file") << ":"
				   << error.getMeta<std::string> (name.str () + "/line") << std::endl;
			}
		}
	}
	catch (kdb::KeyTypeConversion const & e)
	{
		os << "Warnings metadata not set correctly by a plugin: " << e.what () << std::endl;
	}

	return os;
}
} // namespace kdb

#endif

#endif

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
#include <keyset.hpp>

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
		// TODO: use C++ binding version of keyMeta
		KeySet meta (ckdb::ksDup (ckdb::keyMeta (error.getKey ())));
		Key parent ("meta:/warnings", KEY_END);
		auto warnings = meta.cut (parent);

		if (warnings.size () == 0)
		{
			return os;
		}

		int total = 0;
		for (auto it = warnings.begin () + 1; it != warnings.end (); ++it)
		{
			auto name = it->getName ();
			if (it->isDirectBelow (parent))
			{
				total++;
			}
		}

		if (total == 0)
		{
			return os;
		}

		os << " Sorry, " << total << " warning" << (total == 1 ? " was" : "s were") << " issued ;(" << std::endl;

		int nr = 1;
		for (auto it = warnings.begin () + 1; it != warnings.end (); ++it)
		{
			auto name = it->getName ();
			if (it->isDirectBelow (parent))
			{
				os << "[" << nr << "] Sorry, module " << warnings.get<std::string> (name + "/module")
				   << " issued the warning " << warnings.get<std::string> (name + "/number") << ":" << std::endl;
				os << "\t" << warnings.get<std::string> (name + "/description") << ": "
				   << warnings.get<std::string> (name + "/reason") << std::endl;
				if (printVerbose)
				{
					os << "\tMountpoint: " << warnings.get<std::string> (name + "/mountpoint") << std::endl;
					os << "\tConfigfile: " << warnings.get<std::string> (name + "/configfile") << std::endl;
				}
				if (printDebug)
				{
					os << "\tAt: " << warnings.get<std::string> (name + "/file") << ":"
					   << warnings.get<std::string> (name + "/line") << std::endl;
				}
				nr++;
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

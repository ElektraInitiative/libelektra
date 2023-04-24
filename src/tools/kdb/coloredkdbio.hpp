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

#include "./ansicolors.hpp"

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
		// TODO: use C++ binding version of keyMeta
		KeySet meta (ckdb::ksDup (ckdb::keyMeta (error.getKey ())));
		Key parent ("meta:/warnings", KEY_END);
		KeySet warnings = meta.cut (parent);
		if (warnings.size () == 0)
		{
			return os;
		}

		// get number of warnings
		Key keyMetaWarnings = warnings.lookup ("meta:/warnings");
		int cntWarnings = 0;
		if (!keyMetaWarnings.isNull () && keyMetaWarnings.isValid ())
		{
			std::string strWarningCount = keyMetaWarnings.getString ();

			// skip leading '#' and '_' characters
			size_t i;
			for (i = 0; i < strWarningCount.length () && (strWarningCount[i] == '#' || strWarningCount[i] == '_'); i++)
				;
			strWarningCount = strWarningCount.substr (i);
			cntWarnings = std::stoi (strWarningCount) + 1;
		}

		os << getErrorColor (ANSI_COLOR::BOLD) << getErrorColor (ANSI_COLOR::MAGENTA) << " Sorry, " << cntWarnings << " warning"
		   << ((cntWarnings == 1) ? " was" : "s were") << " issued ;(" << getErrorColor (ANSI_COLOR::RESET) << std::endl;

		cntWarnings = 0;
		for (auto it = warnings.begin () + 1; it != warnings.end (); ++it)
		{

			if (it->isDirectBelow (parent))
			{
				auto name = it->getName ();

				os << ' ' << ++cntWarnings << ": Module " << getErrorColor (ANSI_COLOR::BOLD)
				   << getErrorColor (ANSI_COLOR::BLUE) << warnings.get<std::string> (name + "/module")
				   << getErrorColor (ANSI_COLOR::RESET) << " issued the warning " << getErrorColor (ANSI_COLOR::BOLD)
				   << getErrorColor (ANSI_COLOR::RED) << warnings.get<std::string> (name + "/number")
				   << getErrorColor (ANSI_COLOR::RESET) << ":" << std::endl;
				os << "\t" << warnings.get<std::string> (name + "/description") << ": "
				   << warnings.get<std::string> (name + "/reason") << std::endl;
				if (printVerbose)
				{
					os << getErrorColor (ANSI_COLOR::BOLD) << "\tMountpoint: " << getErrorColor (ANSI_COLOR::RESET)
					   << warnings.get<std::string> (name + "/mountpoint") << std::endl;
					os << getErrorColor (ANSI_COLOR::BOLD) << "\tConfigfile: " << getErrorColor (ANSI_COLOR::RESET)
					   << warnings.get<std::string> (name + "/configfile") << std::endl;
				}
				if (printDebug)
				{
					os << getErrorColor (ANSI_COLOR::BOLD) << "\tAt: " << getErrorColor (ANSI_COLOR::RESET)
					   << warnings.get<std::string> (name + "/file") << ":"
					   << warnings.get<std::string> (name + "/line") << std::endl;
				}
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

/**
 * @file
 *
 * @brief This examples show how Elektra’s KDBException can be changed in a way so that it has user defined output.
 *
 *
 * It works -- because of binary compatibility -- if only the receiver
 * of the message (where it is catched) redefines the IO functions
 * printError and printWarnings. They need to be defined with the
 * same signature and in either global or kdb namespace.
 *
 * The output operators of Key and KeySet can be redefined without any
 * macro by simply not including \<keyio.hpp\> and \<keysetio.hpp\>.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#define USER_DEFINED_IO

#include <iomanip>
#include <iostream>

#include <key.hpp>
#include <keyset.hpp>

inline std::ostream & printError (std::ostream & os, kdb::Key const & error, bool printVerbose, bool printDebug)
{
	os << "User defined IO (errors)" << std::endl;

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
		os << "Error metadata is not set correctly by a plugin" << std::endl;
	}

	return os;
}

inline std::ostream & printWarnings (std::ostream & os, kdb::Key const & error, bool printVerbose, bool printDebug)
{
	os << "User defined IO (warnings)" << std::endl;

	try
	{
		// TODO: use C++ binding version of keyMeta
		kdb::KeySet meta (ckdb::ksDup (ckdb::keyMeta (error.getKey ())));
		kdb::Key parent ("meta:/warnings", KEY_END);
		auto warnings = meta.cut (parent);

		if (warnings.size () == 0)
		{
			return os;
		}
		else if (warnings.size () == 1)
		{
			os << "1 Warning was issued:" << std::endl;
		}
		else
		{
			os << warnings.size () << " Warnings were issued:" << std::endl;
		}

		for (auto it = warnings.begin () + 1; it != warnings.end (); ++it)
		{
			auto name = it->getName ();
			if (it->isDirectBelow (parent))
			{
				os << "\tSorry, module " << warnings.get<std::string> (name + "/module") << " issued the warning "
				   << warnings.get<std::string> (name + "/number") << ":" << std::endl;
				os << "\t" << warnings.get<std::string> (name + "/description") << ": "
				   << warnings.get<std::string> (name + "/reason") << std::endl;
				// os << "\t" << name << ": " << warnings.get<std::string>(name) << std::endl;
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
			}
		}
	}
	catch (kdb::KeyTypeConversion const & e)
	{
		os << "Warnings metadata not set correctly by a plugin" << std::endl;
	}

	return os;
}

#include <kdb.hpp>

#include <kdbio.hpp>
#include <keyio.hpp>
#include <keysetio.hpp>

int main ()
{
	kdb::Key k ("user:/sw/MyApp", KEY_END);
	std::cout << k << std::endl;

	kdb::KeySet ks;
	ks.append (k);
	std::cout << ks;

	try
	{
		kdb::KDB kdb (k);
		kdb.get (ks, k);

		std::cout << ks;

		kdb.set (ks, k);
		kdb.close (k);
		printWarnings (std::cout, k, false, false);
	}
	catch (kdb::KDBException const & e)
	{
		std::cout << e.what (); // will print user defined IO
	}
}

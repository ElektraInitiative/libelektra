/**
 * @file
 *
 * @brief Common Elektra extensions for GTest
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 *
 */

#include "gtest/gtest.h"

#include <backend.hpp>
#include <backends.hpp>

#include <kdbconfig.h>

namespace testing
{

class Namespaces
{
public:
	struct Namespace
	{
		std::string name;
	};

	Namespaces ()
	{
		Namespace n;
		n.name = "system";
		namespaces.push_back (n);

		n.name = "spec";
		namespaces.push_back (n);

		n.name = "user";
		namespaces.push_back (n);
	}

	Namespace & operator[] (size_t i)
	{
		return namespaces[i];
	}

	size_t size ()
	{
		return namespaces.size ();
	}

	std::vector<Namespace> namespaces;
};

/**
 * @brief Cascading + Spec mountpoint
 *
 * Hardcoded with resolver+dump
 *
 * useful to quickly mount something in tests
 * and determine the config file paths (+unlink provided)
 */
class Mountpoint
{
public:
	std::string mountpoint;
	std::string userConfigFile;
	std::string specConfigFile;
	std::string systemConfigFile;
	std::string dirConfigFile; // currently unused, but may disturb tests if present

	Mountpoint (std::string mountpoint_, std::string configFile_) : mountpoint (mountpoint_)
	{
		unlink ();
		mount (mountpoint, configFile_);
		mount ("spec:" + mountpoint, configFile_);

		userConfigFile = getConfigFileName ("user", mountpoint);
		specConfigFile = getConfigFileName ("spec", mountpoint);
		systemConfigFile = getConfigFileName ("system", mountpoint);
		dirConfigFile = getConfigFileName ("dir", mountpoint);
		// std::cout << "config files are: " << dirConfigFile << " "
		// 	<< userConfigFile << " " << specConfigFile << " "
		// 	<< systemConfigFile << std::endl;
	}

	~Mountpoint ()
	{
		umount ("spec:" + mountpoint);
		umount (mountpoint);

		unlink ();
	}

	void unlink ()
	{
		::unlink (userConfigFile.c_str ());
		::unlink (systemConfigFile.c_str ());
		::unlink (specConfigFile.c_str ());
	}

	static std::string getConfigFileName (std::string ns, std::string mp)
	{
		using namespace kdb;
		using namespace kdb;
		using namespace kdb::tools;

		using namespace kdb::tools;

		KDB kdb;
		Key parent (ns + ":/" + mp, KEY_END);
		KeySet ks;
		kdb.get (ks, parent);
		return parent.getString ();
	}

	static void mount (std::string mountpoint_, std::string configFile)
	{
		using namespace kdb;
		using namespace kdb::tools;

		Backend b;
		b.setMountpoint (Key (mountpoint_, KEY_END), KeySet (0, KS_END));
		b.addPlugin (PluginSpec (KDB_RESOLVER));
		b.useConfigFile (configFile);
		b.addPlugin (PluginSpec ("dump"));
		b.addPlugin (PluginSpec ("error"));
		KeySet ks;
		KDB kdb;
		Key parentKey ("system:/elektra/mountpoints", KEY_END);
		kdb.get (ks, parentKey);
		b.serialize (ks);
		kdb.set (ks, parentKey);
	}

	static void umount (std::string mountpoint_)
	{
		using namespace kdb;
		using namespace kdb::tools;
		KeySet ks;
		KDB kdb;
		Key parentKey ("system:/elektra/mountpoints", KEY_END);
		kdb.get (ks, parentKey);
		Backends::umount (mountpoint_, ks);
		kdb.set (ks, parentKey);
	}
};

std::string makeLiteralString (std::string str)
{
	std::string ret;
	for (size_t i = 0; i < str.length (); ++i)
	{
		if (str[i] == '\\')
		{
			ret += "\\\\";
		}
		else
		{
			ret += str[i];
		}
	}
	return ret;
}

typedef std::unique_ptr<testing::Mountpoint> MountpointPtr;


void outputGTest (kdb::KeySet tocheck, std::string name)
{
	std::cout << "ASSERT_EQ(" << name << ".size(), " << tocheck.size () << ") << \"wrong size\" << ks;" << std::endl;
	std::cout << name << ".rewind();" << std::endl;

	for (kdb::Key k : tocheck)
	{
		std::cout << name << " iteration" << std::endl;
		std::cout << "EXPECT_EQ(" << name << "k.getName(), \"" << makeLiteralString (k.getName ())
			  << "\") << \"name of element in keyset wrong\";" << std::endl;
		std::cout << "EXPECT_EQ(" << name << "k.getString(), \"" << makeLiteralString (k.getString ())
			  << "\") << \"string of element in keyset wrong\";" << std::endl;
	}
}
} // namespace testing

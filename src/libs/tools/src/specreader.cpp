#include <specreader.hpp>

#include <backendbuilder.hpp>

#include <unordered_set>


namespace kdb
{

namespace tools
{

SpecBackendBuilder::SpecBackendBuilder(BackendBuilderInit const & bbi) :
	MountBackendBuilder (bbi),
	nodes(0)
{}


SpecReader::SpecReader(BackendBuilderInit const & abbi) :
	bbi(abbi)
{}

SpecReader::~SpecReader()
{}

bool startsWith(std::string const & str, std::string const & start)
{
	return std::equal(start.begin(), start.end(), str.begin());
}

SpecBackendBuilder SpecReader::readMountpointSpecification (KeySet const & cks)
{
	KeySet ks (cks);
	SpecBackendBuilder bb (bbi);
	Key mp = ks.head().dup();
	bb.setMountpoint (mp, mountConf);
	bb.useConfigFile (mp.getMeta<std::string>("mountpoint"));

	ks.lookup(mp, KDB_O_POP);
	bb.nodes ++; // count mp

	KeySet backendConfig;

	ks.rewind(); // we need old fashioned loop, because it can handle ks.cut during iteration
	for (Key k = ks.next(); k; k = ks.next())
	{
		// search for mountpoint
		Key m = k.getMeta<const Key>("mountpoint");
		if (m)
		{
			backends[k] = readMountpointSpecification(ks.cut(k));
			continue;
		}

		bb.nodes ++;

		k.rewindMeta();
		while ((m = k.nextMeta()))
		{
			std::string const & cn = "config/needs";
			std::string const & cp = "config/plugin";
			if (startsWith (m.getName(), cn))
			{
				Key bKey = m.dup();
				bKey.setName ("user"+bKey.getName().substr(cn.length()));
				backendConfig.append (bKey);
			}
			else if (startsWith(m.getName(), cp))
			{
				Key bKey = m.dup();
				bKey.setName ("user"+bKey.getName().substr (cp.length()));
				std::string pluginName = m.getName().substr (cp.length());
				PluginSpec toInsert (pluginName, KeySet(1, *bKey, KS_END));
				bb.addPlugin (toInsert);
			}
			else if (m.getName() == "info/needs")
			{
				std::istringstream is (m.getString());
				std::string toInsert;
				while (is >> toInsert)
				{
					bb.addPlugin (PluginSpec(toInsert));
				}
			}
			else if (m.getName() == "info/recommends")
			{
				// TODO: give user a chance to ignore recommends
				std::istringstream is (m.getString());
				std::string toInsert;
				while (is >> toInsert)
				{
					bb.addPlugin(PluginSpec(toInsert));
				}
			}
			else if (startsWith (m.getName(), "infos") ||
				 startsWith (m.getName(), "exports") ||
				 startsWith (m.getName(), "constants") ||
				 startsWith (m.getName(), "exports") ||
				 startsWith (m.getName(), "config"))
			{
				// ignore any other infos from contract
			}
			else
			{
				// TODO: should be disfavoured compared to manually listed plugins
				// (especially recommendations should win)
				// order of commands should not matter
				std::istringstream is (m.getString());
				std::string metadata;
				while (is >> metadata)
				{
					PluginSpec plugin (getPluginDatabase()->lookupMetadata (metadata));
					bb.addPlugin (plugin);
				}
			}
		}
	}
	bb.setBackendConfig (backendConfig);
	return bb;
}

void SpecReader::readSpecification (KeySet const & cks)
{
	KeySet ks (cks);
	Key mp;

	ks.rewind(); // we need old fashioned loop, because it can handle ks.cut during iteration
	for (Key k = ks.next(); k; k = ks.next())
	{
		// search for mountpoint
		Key m = k.getMeta<const Key>("mountpoint");
		if (m)
		{
			backends[k] = readMountpointSpecification(ks.cut(k));
		}
	}
}

}

}


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

class SpecMountpointReader
{
private:
	KeySet ks;
	Key mp;
	KeySet backendConfig;
	KeySet mountConf;
	typedef std::unordered_map<Key, SpecBackendBuilder> Backends;
	Backends & backends;
	BackendBuilderInit const & bbi;
	SpecBackendBuilder bb;

public:
	SpecMountpointReader (Backends & b, BackendBuilderInit const & bbi_) :
		backends(b),
		bbi (bbi_),
		bb (bbi)
	{
	}

	void addPlugins (std::string const & plugins);
	void processKey (Key const & ck);
	SpecBackendBuilder readMountpointSpecification (KeySet const & cks);
};

/**
 * @brief Small helper to add a string with space separated plugin names
 *
 * @param plugins a space separated list of plugins
 */
void SpecMountpointReader::addPlugins (std::string const & plugins)
{
	std::istringstream is (plugins);
	std::string toInsert;
	while (is >> toInsert)
	{
		bb.addPlugin (PluginSpec(toInsert));
	}
}

namespace
{
bool isToBeIgnored (std::string const & name)
{
	// TODO: read from METADATA.ini
	return  startsWith (name, "infos") ||
		startsWith (name, "exports") ||
		startsWith (name, "constants") ||
		startsWith (name, "exports") ||

		startsWith (name, "fallback") ||
		startsWith (name, "override") ||
		startsWith (name, "namespace") ||
		name == "default" ||
		name == "context" ||

		startsWith (name, "callback") ||

		startsWith (name, "binary") ||

		startsWith (name, "opt") ||
		startsWith (name, "env") ||

		startsWith (name, "comment") ||
		startsWith (name, "description") ||
		startsWith (name, "see") ||
		startsWith (name, "rationale") ||
		startsWith (name, "description") ||

		name == "mountpoint" ||
		startsWith (name, "config"
		);
}
}

void SpecMountpointReader::processKey (Key const & ck)
{
	Key k (ck);
	k.rewindMeta();
	Key m;
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
			// TODO: parse plugin
			Key bKey = m.dup();
			bKey.setName ("user"+bKey.getName().substr (cp.length()));
			std::string pluginName = m.getName().substr (cp.length());
			PluginSpec toInsert (pluginName, KeySet(1, *bKey, KS_END));
			bb.addPlugin (toInsert);
		}
		else if (m.getName() == "info/needs")
		{
			addPlugins(m.getString());
			// bb.needPlugin(m.getString());
		}
		else if (m.getName() == "info/recommends")
		{
			addPlugins(m.getString());
			// TODO: give user a chance to ignore recommends:
			// bb.recommendPlugin(m.getString());
		}
		else if (isToBeIgnored (m.getName()))
		{}
		else
		{
			bb.needMetadata(m.getName());
		}
	}
}

SpecBackendBuilder SpecMountpointReader::readMountpointSpecification (KeySet const & cks)
{
	ks = cks;
	mp = ks.head().dup();

	bb.setMountpoint (mp, mountConf);
	bb.useConfigFile (mp.getMeta<std::string>("mountpoint"));

	processKey (mp);
	bb.nodes ++; // count mp

	ks.lookup (mp, KDB_O_POP);

	ks.rewind(); // we need old fashioned loop, because it can handle ks.cut during iteration
	for (Key k = ks.next(); k; k = ks.next())
	{
		// search for mountpoint
		Key m = k.getMeta<const Key>("mountpoint");
		if (m)
		{
			SpecMountpointReader smr (backends, bbi);
			backends[k] = smr.readMountpointSpecification(ks.cut(k));
			continue;
		}

		processKey (k);
		bb.nodes ++;
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
			SpecMountpointReader smr (backends, bbi);
			backends[k] = smr.readMountpointSpecification(ks.cut(k));
		}
	}
}

}

}


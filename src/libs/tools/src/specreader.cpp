#include <specreader.hpp>

#include <backendbuilder.hpp>

#include <helper/keyhelper.hpp>

#include <unordered_set>


namespace kdb
{

namespace tools
{

SpecBackendBuilder::SpecBackendBuilder (BackendBuilderInit const & bbi) : MountBackendBuilder (bbi), nodes (0) {}


SpecReader::SpecReader (BackendBuilderInit const & abbi) : bbi (abbi) {}

SpecReader::~SpecReader () {}

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
	SpecMountpointReader (Backends & b, BackendBuilderInit const & bbi_) : backends (b), bbi (bbi_), bb (bbi) {}

	void processKey (Key const & ck);
	SpecBackendBuilder readMountpointSpecification (KeySet const & cks);
};

namespace
{
bool startsWith (std::string const & str, std::string const & start) { return std::equal (start.begin (), start.end (), str.begin ()); }

bool isToBeIgnored (std::string const & name)
{
	// TODO: read from METADATA.ini
	return startsWith (name, "infos") || startsWith (name, "exports") || startsWith (name, "constants") ||
	       startsWith (name, "exports") ||

	       startsWith (name, "fallback") || startsWith (name, "override") || startsWith (name, "namespace") || name == "default" ||
	       name == "context" ||

	       startsWith (name, "callback") ||

	       startsWith (name, "binary") ||

	       // code generator
	       startsWith (name, "type") || startsWith (name, "opt") || startsWith (name, "env") ||

	       // docu
	       startsWith (name, "comment") || startsWith (name, "description") || startsWith (name, "see") ||
	       startsWith (name, "rationale") || startsWith (name, "example") ||

	       name == "mountpoint" || startsWith (name, "config");
}
}

void SpecMountpointReader::processKey (Key const & ck)
{
	Key k (ck);
	k.rewindMeta ();
	Key m;
	while ((m = k.nextMeta ()))
	{
		std::string const & cn = "config/needs";
		if (startsWith (m.getName (), cn))
		{
			Key bKey = m.dup ();
			bKey.setName ("user" + bKey.getName ().substr (cn.length ()));
			backendConfig.append (bKey);
		}
		else if (m.getName () == "infos/plugins")
		{
			bb.addPlugins (parseArguments (m.getString ()));
		}
		else if (m.getName () == "infos/needs")
		{
			bb.needPlugin (m.getString ());
		}
		else if (m.getName () == "infos/recommends")
		{
			bb.recommendPlugin (m.getString ());
		}
		else if (isToBeIgnored (m.getName ()))
		{
		}
		else
		{
			bb.needMetadata (m.getName ());
		}
	}
}

SpecBackendBuilder SpecMountpointReader::readMountpointSpecification (KeySet const & cks)
{
	ks = cks;
	mp = ks.head ().dup ();

	Key rmp (mp.dup ());
	helper::removeNamespace (rmp);

	bb.setMountpoint (rmp, mountConf);

	processKey (mp);
	bb.nodes++; // count mp

	ks.lookup (mp, KDB_O_POP);

	ks.rewind (); // we need old fashioned loop, because it can handle ks.cut during iteration
	for (Key k = ks.next (); k; k = ks.next ())
	{
		// search for mountpoint
		Key m = k.getMeta<const Key> ("mountpoint");
		if (m)
		{
			SpecMountpointReader smr (backends, bbi);
			backends[k] = smr.readMountpointSpecification (ks.cut (k));
			continue;
		}

		processKey (k);
		bb.nodes++;
	}

	bb.setBackendConfig (backendConfig);
	bb.useConfigFile (mp.getMeta<std::string> ("mountpoint"));
	return bb;
}

void SpecReader::readSpecification (KeySet const & cks)
{
	KeySet ks (cks);
	Key mp;

	ks.rewind (); // we need old fashioned loop, because it can handle ks.cut during iteration
	for (Key k = ks.next (); k; k = ks.next ())
	{
		// search for mountpoint
		Key m = k.getMeta<const Key> ("mountpoint");
		if (m)
		{
			SpecMountpointReader smr (backends, bbi);
			backends[k] = smr.readMountpointSpecification (ks.cut (k));
		}
	}
}
}
}

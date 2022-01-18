#include <specreader.hpp>

#include <backendbuilder.hpp>

#include <helper/keyhelper.hpp>

#include "../../../tools/kdb/command.hpp"
#include <unordered_set>


namespace kdb
{

namespace tools
{

const std::set<std::string> supportedTypes{ "enum",
					    "short",
					    "unsigned_short",
					    "long",
					    "unsigned_long",
					    "long_long",
					    "unsigned_long_long",
					    "float",
					    "double",
					    "long_double"
					    "char",
					    "boolean",
					    "octet",
					    "any",
					    "string",
					    "struct_ref",
					    "struct" };

SpecBackendBuilder::SpecBackendBuilder (BackendBuilderInit const & bbi) : MountBackendBuilder (bbi), nodes (0)
{
}


SpecReader::SpecReader (BackendBuilderInit const & abbi) : bbi (abbi)
{
}

SpecReader::~SpecReader ()
{
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
	SpecMountpointReader (Backends & b, BackendBuilderInit const & bbi_) : backends (b), bbi (bbi_), bb (bbi)
	{
	}

	void processKey (Key const & ck);
	SpecBackendBuilder readMountpointSpecification (KeySet const & cks);
};

namespace
{
bool startsWith (std::string const & str, std::string const & start)
{
	if (str.length () < start.length ())
	{
		return false;
	}
	return std::equal (start.begin (), start.end (), str.begin ());
}

bool isToBeIgnored (std::string const & metaName)
{
	const auto & name = metaName.substr (sizeof ("meta:/") - 1);
	// TODO: read from METADATA.ini
	return startsWith (name, "infos") || startsWith (name, "exports") || startsWith (name, "constants") ||
	       startsWith (name, "exports") ||

	       // spec-plugin
	       startsWith (name, "conflict") || startsWith (name, "require") || startsWith (name, "array") ||

	       startsWith (name, "fallback") || startsWith (name, "override") || startsWith (name, "namespace") || name == "default" ||
	       name == "context" ||

	       // always ignore stuff internal to plugins
	       startsWith (name, "internal") ||

	       startsWith (name, "callback") ||

	       startsWith (name, "binary") ||

	       // elektraGetOpts
	       startsWith (name, "opt") || startsWith (name, "args") || startsWith (name, "env") || startsWith (name, "command") ||

	       // code generator
	       startsWith (name, "gen") ||

	       // docu
	       startsWith (name, "comment") || startsWith (name, "description") || startsWith (name, "see") ||
	       startsWith (name, "rationale") || startsWith (name, "example") ||

	       name == "mountpoint" || startsWith (name, "config");
}
} // namespace

void SpecMountpointReader::processKey (Key const & ck)
{
	Key k (ck);
	k.rewindMeta ();
	Key m;
	while ((m = k.nextMeta ()))
	{
		std::string const & cn = "meta:/config/needs";
		if (startsWith (m.getName (), cn))
		{
			Key bKey = m.dup ();
			bKey.setName ("user:" + bKey.getName ().substr (cn.length ()));
			backendConfig.append (bKey);
		}
		else if (m.getName () == "meta:/infos/plugins")
		{
			bb.addPlugins (parseArguments (m.getString ()));
		}
		else if (m.getName () == "meta:/infos/needs")
		{
			bb.needPlugin (m.getString ());
		}
		else if (m.getName () == "meta:/infos/recommends")
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
	KeySet ks;
	Key mp;

	// Filter keys and perform sanity checks.
	for (Key k : cks)
	{
		// Only include keys in spec namespace
		if (k.isSpec ())
		{
			ks.append (k);
		}
		checkKey (k);
	}

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

void SpecReader::checkKey (const Key key)
{
	std::ostringstream stringStream;
	// Ensure that "check/enum" can only be used with type "enum"
	if (key.getMeta<std::string> ("type") != "enum" && key.hasMeta ("check/enum"))
	{
		stringStream << "Key " << key.getName () << " has \"type\"=\"" << key.getMeta<std::string> ("type")
			     << "\" and \"check/enum\". \"check/enum\" can only be used with \"type=enum\"!";
	}
	// Checks for "type" and "check/type".
	else if (key.hasMeta ("type"))
	{
		std::string keyType = key.getMeta<std::string> ("type");
		// Check if "type" is supported
		if (std::find (supportedTypes.begin (), supportedTypes.end (), key.getMeta<std::string> ("type")) == supportedTypes.end ())
		{
			stringStream << "Type \"" << key.getMeta<std::string> ("type") << "\" of key \"" << key.getName ()
				     << "\" is not supported in Elektra!";
		}
		// Check if "type" and "check/type" are equal.
		else if (key.hasMeta ("check/type") && key.getMeta<std::string> ("check/type") != keyType)
		{
			// If type is "struct" or "struct_ref", it may also have "check/type"="any". See file
			// doc/help/elektra-highlevel-gen.md.
			if (!((keyType == "struct" || keyType == "struct_ref") && key.getMeta<std::string> ("check/type") == "any"))
			{
				stringStream << "Key " << key.getName ()
					     << " has different values for \"type\" and \"check/type\". If both are specified, they must "
						"be equal!";
			}
		}
	}
	if (stringStream.str ().length () > 0)
	{
		throw CommandAbortException (stringStream.str ());
	}
}

} // namespace tools
} // namespace kdb

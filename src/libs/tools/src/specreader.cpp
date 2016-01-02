#include <specreader.hpp>

#include <backendbuilder.hpp>


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
			if (m.getName().find(cn) == 0)
			{
				Key bKey = m.dup();
				bKey.setName ("user"+bKey.getName().substr(cn.length()));
				backendConfig.append(bKey);
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


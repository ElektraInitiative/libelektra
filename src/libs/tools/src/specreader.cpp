#include <specreader.hpp>

#include <backendbuilder.hpp>


namespace kdb
{

namespace tools
{

SpecBackendBuilder::SpecBackendBuilder(BackendBuilderInit const & bbi) :
	BackendBuilder (bbi),
	nodes(0)
{}

SpecReader::SpecReader() :
	pluginDatabase(new ModulesPluginDatabase)
{
}

SpecReader::SpecReader(PluginDatabasePtr const & pd) :
	pluginDatabase(pd)
{}

SpecReader::~SpecReader()
{}

void SpecReader::readSpecification (KeySet const & ks)
{
	Key mp;
	for (auto const & k : ks)
	{
		// search for mountpoint
		Key m = k.getMeta<const Key>("mountpoint");
		if (m)
		{
			mp = k.dup();
			mp.setString(m.getMeta<std::string>("mountpoint"));
			backends[mp] = SpecBackendBuilder(bbi);
			backends[mp].nodes ++;
		}

		// TODO: handle recursive mountpoint with cutting

		if (k.isBelow(mp))
		{
			backends[mp].nodes ++;
		}
	}
}

}

}


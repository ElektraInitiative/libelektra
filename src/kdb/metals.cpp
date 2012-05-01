#include <metals.hpp>

#include <iostream>

#include <kdb.hpp>

using namespace kdb;
using namespace std;

MetaLsCommand::MetaLsCommand()
{}

int MetaLsCommand::execute (Cmdline const& cl)
{
	Key root (cl.arguments[0], KEY_END);
	if (!root.isValid())
	{
		cerr << "Not a valid name supplied" << endl;
		return 1;
	}

	kdb.get(ks, root);

	Key k = ks.lookup(root);
	if (k)
	{
		k.rewindMeta();
		while (const Key meta = k.nextMeta())
		{
			cout << meta.getName() << endl;
		}
	}

	printWarnings(root);

	return 0;
}

MetaLsCommand::~MetaLsCommand()
{}

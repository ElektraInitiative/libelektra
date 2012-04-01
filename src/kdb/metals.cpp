#include <metals.hpp>

#include <iostream>

#include <kdb.hpp>

using namespace kdb;
using namespace std;

MetaLsCommand::MetaLsCommand()
{}

int MetaLsCommand::execute(int argc, char** argv)
{
	if (argc != 3)
	{
		cerr << "Please provide a name" << endl;
		cerr << "Usage: meta-ls <name>" << endl;
		return 1;
	}

	Key root (argv[2], KEY_END);
	if (!root.isValid())
	{
		cerr << "Not a valid name supplied" << endl;
		return 1;
	}

	kdb.get(ks, root);

	KeySet part (ks.cut(root));
	part.rewind();
	Key k;
	while (k=part.next())
	{
		cout << k << endl;
		k.rewindMeta();
		while (const Key meta = k.nextMeta())
		{
			cout << "meta " << meta << endl;
		}
	}

	printWarnings(root);

	return 0;
}

MetaLsCommand::~MetaLsCommand()
{}

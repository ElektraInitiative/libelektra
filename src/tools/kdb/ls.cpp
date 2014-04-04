#include <ls.hpp>

#include <iostream>

#include <kdb.hpp>
#include <cmdline.hpp>
#include <print.hpp>

using namespace kdb;
using namespace std;

LsCommand::LsCommand()
{}

int LsCommand::execute(Cmdline const& cl)
{
	if (cl.arguments.size() != 1)
	{
		throw invalid_argument("1 argument required");
	}

	Key root (cl.arguments[0], KEY_END);
	if (!root.isValid())
	{
		throw invalid_argument(cl.arguments[0] +
				" is not a valid keyname");
	}

	kdb.get(ks, root);

	if (cl.verbose) cout << "size of all keys in mountpoint: " << ks.size() << endl;

	KeySet part (ks.cut(root));

	if (cl.verbose) cout << "size of requested keys: " << part.size() << endl;

	cout << part;

	printWarnings(root);

	return 0;
}

LsCommand::~LsCommand()
{}

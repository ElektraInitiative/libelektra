#include <get.hpp>

#include <kdb.hpp>
#include <cmdline.hpp>

#include <kdbconfig.h>
#include <kdbproposal.h> // for some options

#include <iostream>

using namespace std;
using namespace kdb;

GetCommand::GetCommand() : kdb(root)
{}

void printOptions(option_t options)
{
	// :'<,'>s/\(.*\)/^Iif(options \& \1) std::cout << "\1 "; 
	if(options & KDB_O_DEL) std::cout << "KDB_O_DEL ";
	if(options & KDB_O_POP) std::cout << "KDB_O_POP ";
	if(options & KDB_O_NODIR) std::cout << "KDB_O_NODIR ";
	if(options & KDB_O_DIRONLY) std::cout << "KDB_O_DIRONLY ";
	if(options & KDB_O_NOREMOVE) std::cout << "KDB_O_NOREMOVE ";
	if(options & KDB_O_REMOVEONLY) std::cout << "KDB_O_REMOVEONLY ";
	if(options & KDB_O_INACTIVE) std::cout << "KDB_O_INACTIVE ";
	if(options & KDB_O_SYNC) std::cout << "KDB_O_SYNC ";
	if(options & KDB_O_SORT) std::cout << "KDB_O_SORT ";
	if(options & KDB_O_NORECURSIVE) std::cout << "KDB_O_NORECURSIVE ";
	if(options & KDB_O_NOCASE) std::cout << "KDB_O_NOCASE ";
	if(options & KDB_O_WITHOWNER) std::cout << "KDB_O_WITHOWNER ";
	if(options & KDB_O_NOALL) std::cout << "KDB_O_NOALL ";

	if(options & ckdb::KDB_O_SPEC) std::cout << "KDB_O_SPEC ";
	if(options & ckdb::KDB_O_CREATE) std::cout << "KDB_O_CREATE ";
	if(options & ckdb::KDB_O_NOCASCADING) std::cout << "KDB_O_NOCASCADING ";
	if(options & ckdb::KDB_O_NOSPEC) std::cout << "KDB_O_NOSPEC ";
	if(options & ckdb::KDB_O_NODEFAULT) std::cout << "KDB_O_NODEFAULT ";
	if(options & ckdb::KDB_O_CALLBACK) std::cout << "KDB_O_CALLBACK";
}


ckdb::Key * warnOnMeta(ELEKTRA_UNUSED ckdb::KeySet *ks, ckdb::Key *key, option_t options)
{
	if (!strncmp(keyName(key), "spec/", 5) && options == ckdb::KDB_O_CALLBACK)
	{
		const ckdb::Key *meta = keyGetMeta(key, "context");
		if (meta)
		{
			std::cout << keyName(key) << " is context dependent, shown result might be wrong, -v shows you the trace to the key" << std::endl;
		}
	}
	return key;
}

ckdb::Key * printTrace (ELEKTRA_UNUSED ckdb::KeySet *ks, ckdb::Key *key, option_t options)
{
	warnOnMeta(ks, key, options);
	Key k(key);
	int depth = k.getMeta<int>("print_trace/depth");
	if (k.getName().substr(0,5) == "spec/" && (options & ckdb::KDB_O_CALLBACK)) k.setMeta<int>("print_trace/depth", ++depth);
	for (int i=0; i<depth; ++i) std::cout << " ";
	std::string lastKeyName = k.getMeta<std::string>("print_trace/last_key_name");
	std::cout << "!!! searching " << k.getName() << " last: " << lastKeyName << " options: ";
	printOptions(options);
	std::cout << std::endl;
	k.setMeta<string>("print_trace/last_key_name", k.getName());
	k.release();
	return key;
}


int GetCommand::execute (Cmdline const& cl)
{
	if (cl.arguments.size() != 1) throw invalid_argument ("Need one argument");

	KeySet conf;
	root.setName(cl.arguments[0]);
	if (!root.isValid())
	{
		throw invalid_argument(cl.arguments[0] + " is not an valid keyname");
	}

	std::string n;
	if (cl.all)
	{
		n = root.getName();
		root.setName("/");
	}

	kdb.get(conf, root);

	if (cl.all)
	{
		root.setName(n);
	}

	if (cl.verbose)
	{
		cout << "got " << conf.size() << " keys" << std::endl;
		root.setCallback(printTrace);
	}
	Key k = conf.lookup(root);

	int ret = 0;

	if (k)
	{
		if (cl.verbose)
		{
			cout << "The resulting keyname is " << k.getName() << std::endl;
		}
		cout << k.getString();
	}
	else
	{
		cerr << "Did not find key";
		ret = 1;
	}

	if (!cl.noNewline)
	{
		cout << endl;
	}

	printWarnings(cerr, root);
	printError(cerr, root);

	return ret;
}

GetCommand::~GetCommand()
{}

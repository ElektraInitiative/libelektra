/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <get.hpp>

#include <cmdline.hpp>
#include <kdb.hpp>

#include <internal/macros/old_utils.h>
#include <internal/utility/old_helper.h>

#include <iostream>

#include <internal/macros/old_utils.h>

using namespace std;
using namespace kdb;

GetCommand::GetCommand ()
{
}

namespace
{

void printOptions (elektraLookupFlags options)
{
	// :'<,'>s/\(.*\)/^Iif(options \& \1) std::cout << "\1 ";
	if (options & ckdb::KDB_O_SPEC) std::cout << "KDB_O_SPEC ";
	if (options & ckdb::KDB_O_CREATE) std::cout << "KDB_O_CREATE ";
	if (options & ckdb::KDB_O_NOCASCADING) std::cout << "KDB_O_NOCASCADING ";
	if (options & ckdb::KDB_O_NOSPEC) std::cout << "KDB_O_NOSPEC ";
	if (options & ckdb::KDB_O_NODEFAULT) std::cout << "KDB_O_NODEFAULT ";
	if (options & ckdb::KDB_O_CALLBACK) std::cout << "KDB_O_CALLBACK";
}


ckdb::Key * warnOnMeta (ELEKTRA_UNUSED ckdb::KeySet * ks, ELEKTRA_UNUSED ckdb::Key * key, ckdb::Key * found, elektraLookupFlags options)
{
	if (found && !strncmp (keyName (found), "spec:/", 5) && options == ckdb::KDB_O_CALLBACK)
	{
		const ckdb::Key * meta = keyGetMeta (found, "context");
		if (meta)
		{
			std::cout << "WARNING " << keyName (found)
				  << " is context dependent, shown result might be wrong, -v shows you the trace to the key" << std::endl;
		}
	}
	return found;
}

std::string getCascadingName (std::string name)
{
	if (name[0] == '/') return name;
	if (name.find ('/') == std::string::npos) return "/";
	return name.substr (name.find ('/'));
}
} // namespace

ckdb::Key * printTrace (ELEKTRA_UNUSED ckdb::KeySet * ks, ckdb::Key * key, ckdb::Key * found, elektraLookupFlags options)
{
	warnOnMeta (ks, key, found, options);

	Key k (key);
	Key f (found);

	std::string lastKeyName = k.getMeta<std::string> ("callback/print_trace/last_key_name");
	int depth = k.getMeta<int> ("callback/print_trace/depth");

	for (int i = 0; i < depth; ++i)
		std::cout << " ";

	std::cout << "searching " << (k.getName ()[0] == '/' ? "default of spec" : "") << k.getName ()
		  << ", found: " << (found ? f.getName () : "<nothing>");

	if (options)
	{
		std::cout << ", options: ";
		printOptions (options);
	}
	std::cout << std::endl;

	if (k.getName ().substr (0, 6) == "spec:/" && (options & ckdb::KDB_O_CALLBACK))
	{
		depth += 4;
		k.setMeta<int> ("callback/print_trace/depth", depth);
	}
	else
	{
		if (getCascadingName (lastKeyName) != getCascadingName (k.getName ()))
		{
			if (depth != 0)
			{
				depth -= 2;
			}
			k.setMeta<int> ("callback/print_trace/depth", depth);
		}
	}
	k.setMeta<string> ("callback/print_trace/last_key_name", k.getName ());

	f.release ();
	k.release ();
	return found;
}


int GetCommand::execute (Cmdline const & cl)
{
	if (cl.arguments.size () != 1) throw invalid_argument ("Need one argument");

	KeySet conf;

	kdb::Key root = cl.createKey (0);
	string parentKeyName = cl.all ? "/" : cl.getParentKey (root).getName ();
	kdb::KDB kdb (root);

	std::string originalName = root.getName ();
	root.setName (parentKeyName);
	kdb.get (conf, root);
	root.setName (originalName);

	// do a lookup without tracer to warm up default cache
	conf.lookup (root);

	root.setCallback (warnOnMeta);
	if (cl.verbose)
	{
		cout << "got " << conf.size () << " keys" << std::endl;
		root.setCallback (printTrace);
	}
	Key k = conf.lookup (root);

	int ret = 0;

	if (k)
	{
		if (cl.verbose)
		{
			if (k.getNamespace () == ElektraNamespace::DEFAULT)
			{
				cout << "The key was not found in any other namespace, taking the default" << std::endl;
			}
			cout << "The resulting keyname is " << k.getName () << std::endl;
			cout << "The resulting value size is " << k.getStringSize () << std::endl;
		}

		if (k.isBinary ())
		{
			if (cl.verbose)
			{
				if (k.getBinarySize () == 0)
				{
					cout << "The key is null." << std::endl;
				}
				else
				{
					cout << "The key is binary." << std::endl;
				}
			}
			cout << std::hex;
			const uint8_t * data = static_cast<const uint8_t *> (k.getValue ());
			for (auto position = 0; position < k.getBinarySize (); position++)
			{
				cout << "\\x" << unsigned (data[position]);
			}
			cout << std::dec;
		}
		else
		{
			cout << k.getString ();
		}
	}
	else
	{
		cerr << "Did not find key '" << root.getName () << "'";
		ret = 11;
	}

	if (!cl.noNewline)
	{
		cout << endl;
	}

	printWarnings (cerr, root, cl.verbose, cl.debug);
	printError (cerr, root, cl.verbose, cl.debug);

	return ret;
}

GetCommand::~GetCommand ()
{
}

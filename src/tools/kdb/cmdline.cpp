/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/LICENSE.md or http://www.libelektra.org)
 */

#include <cmdline.hpp>

#include <backendparser.hpp>
#include <kdb.hpp>
#include <kdbconfig.h>
#include <keysetget.hpp>
#include <keysetio.hpp>

#include <cstdio>
#include <iostream>
#include <set>
#include <vector>

#include <getopt.h>

#include <command.hpp>
#include <external.hpp>

#ifndef _WIN32
#include <sys/types.h>
#include <unistd.h>
#endif


using namespace std;

Cmdline::Cmdline (int argc, char ** argv, Command * command)
: helpText (), invalidOpt (false),

  /*XXX: Step 2: initialise your option here.*/
  debug (), force (), load (), humanReadable (), help (), interactive (), minDepth (0), maxDepth (1), noNewline (), test (), recursive (),
  resolver (KDB_RESOLVER), strategy ("preserve"), verbose (), quiet (), version (), withoutElektra (), null (), first (true), second (true),
  third (true), withRecommends (false), all (), format (KDB_STORAGE), plugins ("sync"), globalPlugins ("spec"), pluginsConfig (""),
  color ("auto"), ns (""), editor (), bookmarks (), profile ("current"),

  executable (), commandName ()
{
	extern int optind;
	extern char * optarg;

	int opt;

	size_t optionPos;

	synopsis = command->getSynopsis ();

	helpText += command->getShortHelpText ();
	helpText += "\n";
	helpText += command->getLongHelpText ();
	helpText += "\n";

	string allOptions = command->getShortOptions ();
	allOptions += "HVp";

	std::set<string::value_type> unique_sorted_chars (allOptions.begin (), allOptions.end ());
	string acceptedOptions (unique_sorted_chars.begin (), unique_sorted_chars.end ());

	vector<option> long_options;
	/*XXX: Step 3: give it a long name.*/
	if (acceptedOptions.find ('a') != string::npos)
	{
		option o = { "all", no_argument, nullptr, 'a' };
		long_options.push_back (o);
		helpText += "-a --all                 Consider all of the keys.\n";
	}
	if (acceptedOptions.find ('d') != string::npos)
	{
		option o = { "debug", no_argument, nullptr, 'd' };
		long_options.push_back (o);
		helpText += "-d --debug               Give debug information or ask debug questions (in interactive mode).\n";
	}
	if (acceptedOptions.find ('f') != string::npos)
	{
		option o = { "force", no_argument, nullptr, 'f' };
		long_options.push_back (o);
		helpText += "-f --force               Force the action to be done.\n";
	}
	if (acceptedOptions.find ('l') != string::npos)
	{
		option o = { "load", no_argument, nullptr, 'f' };
		long_options.push_back (o);
		helpText += "-l --load                Load plugin even if system/elektra is available\n";
	}
	if (acceptedOptions.find ('h') != string::npos)
	{
		option o = { "human-readable", no_argument, nullptr, 'h' };
		long_options.push_back (o);
		helpText += "-h --human-readable      Print numbers in an human readable way\n";
	}
	if (acceptedOptions.find ('H') != string::npos)
	{
		option o = { "help", no_argument, nullptr, 'H' };
		long_options.push_back (o);
		helpText += "-H --help                Show the man page.\n";
	}
	if (acceptedOptions.find ('i') != string::npos)
	{
		option o = { "interactive", no_argument, nullptr, 'i' };
		long_options.push_back (o);
		helpText += "-i --interactive         Ask the user interactively.\n";
	}
	if (acceptedOptions.find ('m') != string::npos)
	{
		option o = { "min-depth", optional_argument, nullptr, 'm' };
		long_options.push_back (o);
		helpText += "-m --min-depth           Specify the minimum depth of completion suggestions (0 by default), exclusive.\n";
	}
	if (acceptedOptions.find ('M') != string::npos)
	{
		option o = { "max-depth", optional_argument, nullptr, 'M' };
		long_options.push_back (o);
		helpText +=
			"-M --max-depth           Specify the maximum depth of completion suggestions (1 by default, -1 to set no limit), "
			"inclusive.\n";
	}
	if (acceptedOptions.find ('n') != string::npos)
	{
		option o = { "no-newline", no_argument, nullptr, 'n' };
		long_options.push_back (o);
		helpText += "-n --no-newline          Suppress the newline at the end of the output.\n";
	}
	if (acceptedOptions.find ('t') != string::npos)
	{
		option o = { "test", no_argument, nullptr, 't' };
		long_options.push_back (o);
		helpText += "-t --test                Test.\n";
	}
	if (acceptedOptions.find ('r') != string::npos)
	{
		option o = { "recursive", no_argument, nullptr, 'r' };
		long_options.push_back (o);
		helpText += "-r --recursive           Work in a recursive mode.\n";
	}
	optionPos = acceptedOptions.find ('R');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "resolver", required_argument, nullptr, 'R' };
		long_options.push_back (o);
		helpText += "-R --resolver <name>     Specify the resolver plugin to use\n";
	}
	optionPos = acceptedOptions.find ('p');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "profile", required_argument, nullptr, 'p' };
		long_options.push_back (o);
		helpText += "-p --profile <name>      Use a different profile for kdb configuration\n";
	}
	optionPos = acceptedOptions.find ('s');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "strategy", required_argument, nullptr, 's' };
		long_options.push_back (o);
		helpText += "-s --strategy <name>     Specify the strategy to resolve conflicts.\n";
	}
	if (acceptedOptions.find ('v') != string::npos)
	{
		option o = { "verbose", no_argument, nullptr, 'v' };
		long_options.push_back (o);
		helpText += "-v --verbose             Explain what is happening.\n";
	}
	if (acceptedOptions.find ('q') != string::npos)
	{
		option o = { "quiet", no_argument, nullptr, 'q' };
		long_options.push_back (o);
		helpText += "-q --quiet               Only print error messages.\n";
	}
	if (acceptedOptions.find ('V') != string::npos)
	{
		option o = { "version", no_argument, nullptr, 'V' };
		long_options.push_back (o);
		helpText += "-V --version             Print version info.\n";
	}
	if (acceptedOptions.find ('E') != string::npos)
	{
		option o = { "without-elektra", no_argument, nullptr, 'E' };
		long_options.push_back (o);
		helpText += "-E --without-elektra     Omit the `/elektra` directory.\n";
	}
	optionPos = acceptedOptions.find ('e');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "editor", required_argument, 0, 'e' };
		long_options.push_back (o);
		helpText += "-e --editor              Which external editor to use.\n";
	}
	if (acceptedOptions.find ('W') != string::npos)
	{
		option o = { "with-recommends", no_argument, nullptr, 'W' };
		long_options.push_back (o);
		helpText += "-W --with-recommends     Add recommended plugins.\n";
	}
	if (acceptedOptions.find ('0') != string::npos)
	{
		option o = { "null", no_argument, nullptr, '0' };
		long_options.push_back (o);
		helpText += "-0 --null                Use binary 0 termination.\n";
	}
	if (acceptedOptions.find ('1') != string::npos)
	{
		option o = { "first", no_argument, nullptr, '1' };
		long_options.push_back (o);
		helpText += "-1 --first               Suppress the first column.\n";
	}
	if (acceptedOptions.find ('2') != string::npos)
	{
		option o = { "second", no_argument, nullptr, '2' };
		long_options.push_back (o);
		helpText += "-2 --second              Suppress the second column.\n";
	}
	if (acceptedOptions.find ('3') != string::npos)
	{
		option o = { "third", no_argument, nullptr, '3' };
		long_options.push_back (o);
		helpText += "-3 --third               Suppress the third column.\n";
	}
	optionPos = acceptedOptions.find ('N');
	if (acceptedOptions.find ('N') != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "namespace", required_argument, nullptr, 'N' };
		long_options.push_back (o);
		helpText += "-N --namespace ns        Specify the namespace to use for cascading keys.\n";
	}
	optionPos = acceptedOptions.find ('c');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "plugins-config", required_argument, nullptr, 'c' };
		long_options.push_back (o);
		helpText += "-c --plugins-config      Add a plugin configuration.\n";
	}
	optionPos = acceptedOptions.find ('C');
	if (optionPos != string::npos)
	{
		option o = { "color", optional_argument, nullptr, 'C' };
		long_options.push_back (o);
		helpText += "-C --color[=WHEN]        Print never/auto(default)/always colored output.\n";
	}

	int index = 0;
	option o = { nullptr, 0, nullptr, 0 };
	long_options.push_back (o);

	executable = argv[0];
	commandName = argv[1];

	opterr = 0;

	while ((opt = getopt_long (argc, argv, acceptedOptions.c_str (), &long_options[0], &index)) != EOF)
	{
		switch (opt)
		{
		case 'p':
			profile = optarg;
			break;
		default: // ignore everything else for now
			break;
		}
	}


	try
	{
		using namespace kdb;
		/*XXX: Step 4: use default from KDB, if available.*/
		KDB kdb;
		KeySet conf;

		for (int i = 0; i <= 2; ++i)
		{
			std::string dirname;
			switch (i)
			{
			// prefer later dirnames (will override)
			case 0:
				dirname = "/sw/kdb/" + profile + "/";
				break; // legacy
			case 1:
				dirname = "/sw/elektra/kdb/#0/%/";
				break; // no profile
			case 2:
				dirname = "/sw/elektra/kdb/#0/" + profile + "/";
				break; // current profile
			}

			kdb.get (conf, dirname);

			Key k = conf.lookup (dirname + "resolver");
			if (k) resolver = k.get<string> ();

			k = conf.lookup (dirname + "format");
			if (k) format = k.get<string> ();

			k = conf.lookup (dirname + "plugins");
			if (k) plugins = k.get<string> ();

			k = conf.lookup (dirname + "plugins/global");
			if (k) globalPlugins = k.get<string> ();

			k = conf.lookup (dirname + "namespace");
			if (k) ns = k.get<string> ();

			k = conf.lookup (dirname + "verbose");
			if (k) verbose = k.get<bool> ();

			k = conf.lookup (dirname + "quiet");
			if (k) quiet = k.get<bool> ();

			k = conf.lookup (dirname + "editor");
			if (k) editor = k.get<string> ();

			k = conf.lookup (dirname + "recommends");
			if (k) withRecommends = k.get<bool> ();

			map nks = conf.get<map> (dirname + "bookmarks");
			bookmarks.insert (nks.begin (), nks.end ());

			k = conf.lookup (dirname + "color");
			if (k) color = k.get<std::string> ();
		}
	}
	catch (kdb::KDBException const & ce)
	{
		std::cerr << "Sorry, I could not fetch my own configuration:\n" << ce.what () << std::endl;
	}

	// reinit
	index = 0;
	optind = 1;

	if (!dynamic_cast<ExternalCommand *> (command))
	{
		// do not print to stderr for external commands,
		// we do not know which options they have and
		// otherwise maybe wrong "invalid/unrecognized option"
		// are reported to stderr.
		opterr = 1;
	}

	while ((opt = getopt_long (argc, argv, acceptedOptions.c_str (), &long_options[0], &index)) != EOF)
	{
		switch (opt)
		{
		/*XXX: Step 5: and now process the option.*/
		case 'a':
			all = true;
			break;
		case 'C':
			if (!optarg)
			{
				color = "auto";
				break;
			}
			color = optarg;
			break;
		case 'd':
			debug = true;
			break;
		case 'e':
			editor = optarg;
			break;
		case 'f':
			force = true;
			break;
		case 'h':
			humanReadable = true;
			break;
		case 'l':
			load = true;
			break;
		case 'H':
			help = true;
			break;
		case 'i':
			interactive = true;
			break;
		case 'm':
			if (!optarg)
			{
				minDepth = 0;
				break;
			}
			stringstream (optarg) >> minDepth;
			break;
		case 'M':
			if (!optarg)
			{
				cout << " no optarg for max " << endl;
				maxDepth = 1;
				break;
			}
			stringstream (optarg) >> maxDepth;
			if (maxDepth == -1)
			{
				maxDepth = numeric_limits<int>::max ();
			}
			break;
		case 'n':
			noNewline = true;
			break;
		case 't':
			test = true;
			break;
		case 'r':
			recursive = true;
			break;
		case 'p':
			break; // already handled above
		case 'R':
			resolver = optarg;
			break;
		case 's':
			strategy = optarg;
			break;
		case 'v':
			verbose = true;
			break;
		case 'q':
			quiet = true;
			break;
		case 'V':
			version = true;
			break;
		case 'E':
			withoutElektra = true;
			break;
		case 'W':
			withRecommends = true;
			break;
		case '0':
			null = true;
			break;
		case '1':
			first = false;
			break;
		case '2':
			second = false;
			break;
		case '3':
			third = false;
			break;
		case 'N':
			ns = optarg;
			break;
		case 'c':
			pluginsConfig = optarg;
			break;

		default:
			invalidOpt = true;
			break;
		}
	}

	if (quiet && verbose)
	{
		std::cout << "Both quiet and verbose is active: will suppress default messages, but print verbose messages" << std::endl;
	}

	if (ns.empty ())
	{
#ifndef _WIN32
		if (getuid () == 0 || geteuid () == 0)
		{
			ns = "system";
		}
		else
		{
			ns = "user";
		}
#else
		ns = "user";
#endif
	}

	optind++; // skip the command name
	while (optind < argc)
	{
		arguments.push_back (argv[optind++]);
	}

	// init colors
	hasStdColor (color);
	hasErrorColor (color);
}

kdb::KeySet Cmdline::getPluginsConfig (string basepath) const
{
	return kdb::tools::parsePluginArguments (pluginsConfig, basepath);
}

/**
 * @brief create a key from argument number pos
 *
 * @param pos the position in cl.arguments that tells us the name of the key to create
 *
 * @throw invalid_argument if the argument is not a valid keyname
 *
 * @return a newly created key from the name found in cl.arguments[pos]
 */
kdb::Key Cmdline::createKey (int pos) const
{
	std::string name = arguments[pos];
	// std::cerr << "Using " << name << std::endl;
	// for (auto const & n : bookmarks) std::cout << "nks: " << n.second << std::endl;
	if (name.empty ())
	{
		throw invalid_argument ("<empty string> is not a valid keyname. Please enter a valid one.");
	}

	if (name[0] == '+')
	{
		size_t found = name.find ('/');
		std::string bookmark;
		std::string restKey;
		if (found != std::string::npos)
		{
			bookmark = name.substr (1, found - 1);
			restKey = name.substr (found, name.length () - found);
		}
		else
		{
			bookmark = name.substr (1, name.length () - 1);
		}
		auto realKeyIt = bookmarks.find (bookmark);
		std::string realKey;
		if (realKeyIt == bookmarks.end ())
		{
			throw invalid_argument ("cannot find bookmark " + bookmark);
		}
		realKey = realKeyIt->second;
		name = realKey + "/" + restKey;
		if (verbose)
		{
			std::cout << "using bookmark " << bookmark << " which is: " << realKey << "-" << restKey << std::endl;
		}
	}

	kdb::Key root (name, KEY_END);

	if (!root.isValid ())
	{
		throw invalid_argument (name + " is not a valid keyname" + "\n\n" +
					"For absolute keys (starting without '/'), please note that only one of the predefined namespaces "
					"can be used (see 'man elektra-namespaces').\n" +
					"Please also ensure that the path is separated by a '/'.\n" +
					"An example for a valid absolute key is user/a/key, and for a valid cascading key /a/key.");
	}

	return root;
}

std::ostream & operator<< (std::ostream & os, Cmdline & cl)
{
	if (cl.invalidOpt)
	{
		os << "Invalid option given\n" << endl;
	}

	os << "Usage: " << cl.executable << " " << cl.commandName << " " << cl.synopsis;
	os << "\n\n" << cl.helpText;
	return os;
}

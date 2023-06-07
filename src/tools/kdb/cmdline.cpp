/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#include <cmdline.hpp>

#include <backendparser.hpp>
#include <kdb.hpp>
#include <kdbconfig.h>
#include <keysetget.hpp>
#include <keysetio.hpp>
#include <mergehelper.hpp>

#include <cstdio>
#include <iostream>
#include <limits>
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
: synopsis (command->getSynopsis ()), helpText (), invalidOpt (false),

  /*XXX: Step 2: initialise your option here.*/
  debug (), force (), load (), humanReadable (), help (), interactive (), minDepth (0), maxDepth (numeric_limits<int>::max ()),
  noNewline (), test (), recursive (), resolver (KDB_RESOLVER), strategy ("preserve"), verbose (), quiet (), version (), withoutElektra (),
  inputFile (""), null (), first (true), second (true), third (true), includeSessionStorage (false), withRecommends (false), all (),
  format (KDB_STORAGE), plugins (""), globalPlugins ("spec"), pluginsConfig (""), color ("auto"), editor (), bookmarks (),
  profile ("current"),

  executable (), commandName ()
{
	extern int optind;
	extern char * optarg;

	int opt;

	size_t optionPos;

	helpText += command->getShortHelpText ();
	helpText += "\n";
	helpText += command->getLongHelpText ();
	helpText += "\n";

	string allOptions = command->getShortOptions ();
	allOptions += "HVCpvd";

	// Make sure to use the unsorted allOptions for getopt to preserve argument chars : and ::
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
		helpText += "-l --load                Load plugin even if system:/elektra is available.\n";
	}
	if (acceptedOptions.find ('h') != string::npos)
	{
		option o = { "human-readable", no_argument, nullptr, 'h' };
		long_options.push_back (o);
		helpText += "-h --human-readable      Print numbers in an human readable way.\n";
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
	optionPos = acceptedOptions.find ('m');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "min-depth", required_argument, nullptr, 'm' };
		long_options.push_back (o);
		helpText += "-m --min-depth <min>     Specify the minimum depth (default 0).\n";
	}
	optionPos = acceptedOptions.find ('M');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "max-depth", required_argument, nullptr, 'M' };
		long_options.push_back (o);
		helpText += "-M --max-depth <max>     Specify the maximum depth (unlimited by default).\n";
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
		helpText += "-R --resolver <name>     Specify the resolver plugin to use.\n";
	}
	optionPos = acceptedOptions.find ('p');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "profile", required_argument, nullptr, 'p' };
		long_options.push_back (o);
		helpText += "-p --profile <name>      Use a different profile for kdb configuration.\n";
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
	optionPos = acceptedOptions.find ('F');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "input-file", required_argument, nullptr, 'F' };
		long_options.push_back (o);
		helpText += "-F --input-file <plugin>=<file>       Load the given file with the given plugin instead of using the KDB.\n";
	}
	optionPos = acceptedOptions.find ('e');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "editor", required_argument, 0, 'e' };
		long_options.push_back (o);
		helpText += "-e --editor <editor>     Which external editor to use.\n";
	}
	if (acceptedOptions.find ('S') != string::npos)
	{
		option o = { "include-recording-session", no_argument, nullptr, 'S' };
		long_options.push_back (o);
		helpText += "-S --include-recording-session     Include recording session in output.\n";
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
	optionPos = acceptedOptions.find ('c');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "plugins-config", required_argument, nullptr, 'c' };
		long_options.push_back (o);
		helpText += "-c --plugins-config <c>  Add a plugin configuration.\n";
	}
	optionPos = acceptedOptions.find ('C');
	if (optionPos != string::npos)
	{
		acceptedOptions.insert (optionPos + 1, ":");
		option o = { "color", required_argument, nullptr, 'C' };
		long_options.push_back (o);
		helpText += "-C --color <when>       Print never/auto(default)/always colored output.\n";
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


	if (profile != "%")
	{
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
			color = optarg;
			if (color != "never" && color != "auto" && color != "always")
			{
				std::cerr << argv[0] << ": -C --color needs never, auto, or always as argument\n";
				invalidOpt = true;
			}
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
		case 'F':
			inputFile = optarg;
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
			try
			{
				minDepth = stoi (optarg);
			}
			catch (std::invalid_argument const & ia)
			{
				std::cerr << argv[0] << ": -m --min-depth needs a valid number as argument\n";
				invalidOpt = true;
			}
			break;
		case 'M':
			try
			{
				maxDepth = stoi (optarg);
			}
			catch (std::invalid_argument const & ia)
			{
				std::cerr << argv[0] << ": -M --max-depth needs a valid number as argument\n";
				invalidOpt = true;
			}

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
		case 'S':
			includeSessionStorage = true;
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
kdb::Key Cmdline::createKey (int pos, bool allowCascading) const
{
	std::string name = arguments[pos];
	// std::cerr << "Using " << name << std::endl;
	// for (auto const & n : bookmarks) std::cout << "nks: " << n.second << std::endl;
	if (name.empty ())
	{
		throw invalid_argument ("<empty string> is not a valid keyname. Please enter a valid one.");
	}

	kdb::Key root (name, KEY_END);

	if (name[0] == '+')
	{
		kdb::Key bookmark = resolveBookmark (name);
		if (!bookmark.isValid ())
		{
			throw invalid_argument ("cannot find bookmark " + bookmark.getName ());
		}
		root = bookmark;
	}

	if (!root.isValid ())
	{
		throw invalid_argument (name + " is not a valid keyname" + "\n\n" +
					"For absolute keys (starting without '/'), please note that only one of the predefined namespaces "
					"can be used (see 'man elektra-namespaces').\n" +
					"Please also ensure that the path is separated by a '/'.\n" +
					"An example for a valid absolute key is user:/a/key, and for a valid cascading key /a/key.");
	}

	if (!allowCascading && root.isCascading ())
	{
		throw invalid_argument ("The key '" + root.getName () +
					"'is a cascading keyname, which is not supported. Please choose a namespace.");
	}

	return root;
}


/**
 * @brief return a parent key to use with kdbGet/kdbSet
 *
 * @param key the key of interest
 *
 * @return a newly created key to use with kdbGet/kdbSet. If -f was specified, a simple copy will be returned, otherwise a copy without a
 * namespace will be returned.
 */
kdb::Key Cmdline::getParentKey (kdb::Key const & key) const
{
	if (force)
	{
		return key.dup ();
	}
	else
	{
		return removeNamespace (key);
	}
}

/**
 * @brief resolve the bookmark with the given name
 *
 * @param bookmark the name of the bookmark to resolve
 *
 * @return a key to the resolved bookmark, or an invalid key if no bookmark with the given name exists
 */
kdb::Key Cmdline::resolveBookmark (std::string name) const
{
	if (!name.empty () && name[0] == '+')
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
		if (realKeyIt != bookmarks.end ())
		{
			realKey = realKeyIt->second;
			name = realKey + "/" + restKey;
			if (verbose)
			{
				std::cout << "using bookmark " << bookmark << " which is: " << realKey << "-" << restKey << std::endl;
			}
			return kdb::Key (name, KEY_END);
		}
	}
	return kdb::Key ();
}

std::ostream & operator<< (std::ostream & os, Cmdline & cl)
{
	if (cl.invalidOpt)
	{
		os << "Sorry, I could not process the given options (see errors above)\n" << endl;
	}

	os << "Usage: " << cl.executable << " " << cl.commandName << " " << cl.synopsis;
	os << "\n\n" << cl.helpText;
	return os;
}

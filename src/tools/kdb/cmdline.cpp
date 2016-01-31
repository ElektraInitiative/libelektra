/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#include <cmdline.hpp>

#include <kdb.hpp>
#include <kdbconfig.h>
#include <keysetio.hpp>
#include <keysetget.hpp>
#include <backendparser.hpp>

#include <iostream>
#include <vector>
#include <cstdio>
#include <set>

#include <getopt.h>

#include <command.hpp>
#include <external.hpp>


using namespace std;

Cmdline::Cmdline (int argc,
		  char** argv,
		  Command *command
		 ) :
	helpText(),
	invalidOpt(false),

	/*XXX: Step 2: initialise your option here.*/
	debug(),
	force(),
	load(),
	humanReadable(),
	help(),
	interactive(),
	noNewline(),
	test(),
	recursive(),
	resolver(KDB_DEFAULT_RESOLVER),
	strategy("preserve"),
	verbose(),
	version(),
	withoutElektra(),
	null(),
	first(true),
	second(true),
	third(true),
	all(),
	format("dump"),
	plugins("sync"),
	pluginsConfig(""),
	ns("user"),
	editor(),
	namedKeys(),

	executable(),
	commandName()
{
	extern int optind;
	extern char *optarg;

	int index = 0;
	int opt;

	size_t optionPos;

	synopsis = command->getSynopsis();

	helpText += command->getShortHelpText();
	helpText += "\n";
	helpText += command->getLongHelpText();
	helpText += "\n";

	string allOptions = command->getShortOptions();
	allOptions += "HV";

	std::set<string::value_type> unique_sorted_chars (allOptions.begin(), allOptions.end());
	string acceptedOptions (unique_sorted_chars.begin(), unique_sorted_chars.end());

	vector<option> long_options;
	/*XXX: Step 3: give it a long name.*/
	if (acceptedOptions.find('a')!=string::npos)
	{
		option o = {"all", no_argument, nullptr, 'a'};
		long_options.push_back(o);
		helpText += "-a --all                 Consider all of the keys.\n";
	}
	if (acceptedOptions.find('d')!=string::npos)
	{
		option o = {"debug", no_argument, nullptr, 'd'};
		long_options.push_back(o);
		helpText += "-d --debug               Give debug information or ask debug questions (in interactive mode).\n";
	}
	if (acceptedOptions.find('f')!=string::npos)
	{
		option o = {"force", no_argument, nullptr, 'f'};
		long_options.push_back(o);
		helpText += "-f --force               Force the action to be done.\n";
	}
	if (acceptedOptions.find('l')!=string::npos)
	{
		option o = {"load", no_argument, nullptr, 'f'};
		long_options.push_back(o);
		helpText += "-l --load                Load plugin even if system/elektra is available\n";
	}
	if (acceptedOptions.find('h')!=string::npos)
	{
		option o = {"human-readable", no_argument, nullptr, 'h'};
		long_options.push_back(o);
		helpText += "-h --human-readable      Print numbers in an human readable way\n";
	}
	if (acceptedOptions.find('H')!=string::npos)
	{
		option o = {"help", no_argument, nullptr, 'H'};
		long_options.push_back(o);
		helpText += "-H --help                Show the man page.\n";
	}
	if (acceptedOptions.find('i')!=string::npos)
	{
		option o = {"interactive", no_argument, nullptr, 'i'};
		long_options.push_back(o);
		helpText += "-i --interactive         Instead of passing all information by parameters\n";
		helpText += "                         ask the user interactively.\n";
	}
	if (acceptedOptions.find('n')!=string::npos)
	{
		option o = {"no-newline", no_argument, nullptr, 'n'};
		long_options.push_back(o);
		helpText += "-n --no-newline          Suppress the newline at the end of the output.\n";
	}
	if (acceptedOptions.find('t')!=string::npos)
	{
		option o = {"test", no_argument, nullptr, 't'};
		long_options.push_back(o);
		helpText += "-t --test                Test.\n";
	}
	if (acceptedOptions.find('r')!=string::npos)
	{
		option o = {"recursive", no_argument, nullptr, 'r'};
		long_options.push_back(o);
		helpText += "-r --recursive           Work in a recursive mode.\n";
	}
	optionPos = acceptedOptions.find('R');
	if (optionPos!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"resolver", required_argument, nullptr, 'R'};
		long_options.push_back (o);
		helpText +=
				"-R --resolver <name>     Specify the resolver plugin to use\n"
				"                         if no resolver is given, the default resolver is used.\n"
				"";
	}
	optionPos = acceptedOptions.find('s');
	if (optionPos!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"strategy", required_argument, nullptr, 's'};
		long_options.push_back(o);
		helpText +=
			"-s --strategy <name>     Specify which strategy should be used to resolve conflicts.\n";
	}
	if (acceptedOptions.find('v')!=string::npos)
	{
		option o = {"verbose", no_argument, nullptr, 'v'};
		long_options.push_back(o);
		helpText += "-v --verbose             Explain what is happening.\n";
	}
	if (acceptedOptions.find('V')!=string::npos)
	{
		option o = {"version", no_argument, nullptr, 'V'};
		long_options.push_back(o);
		helpText += "-V --version             Print version info.\n";
	}
	if (acceptedOptions.find('E')!=string::npos)
	{
		option o = {"without-elektra", no_argument, nullptr, 'E'};
		long_options.push_back(o);
		helpText += "-E --without-elektra     Omit the `system/elektra` directory.\n";
	}
	optionPos = acceptedOptions.find('e');
	if (optionPos!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"editor", required_argument, 0, 'e'};
		long_options.push_back(o);
		helpText += "-e --editor              Which external editor to use.\n";
	}
	if (acceptedOptions.find('0')!=string::npos)
	{
		option o = {"null", no_argument, nullptr, '0'};
		long_options.push_back(o);
		helpText += "-0 --null                Use binary 0 termination.\n";
	}
	if (acceptedOptions.find('1')!=string::npos)
	{
		option o = {"first", no_argument, nullptr, '1'};
		long_options.push_back(o);
		helpText += "-1 --first               Suppress the first column.\n";
	}
	if (acceptedOptions.find('2')!=string::npos)
	{
		option o = {"second", no_argument, nullptr, '2'};
		long_options.push_back(o);
		helpText += "-2 --second              Suppress the second column.\n";
	}
	if (acceptedOptions.find('3')!=string::npos)
	{
		option o = {"third", no_argument, nullptr, '3'};
		long_options.push_back(o);
		helpText += "-3 --third               Suppress the third column.\n";
	}
	optionPos = acceptedOptions.find('N');
	if (acceptedOptions.find('N')!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"namespace", required_argument, nullptr, 'N'};
		long_options.push_back(o);
		helpText += "-N --namespace ns        Specify the namespace to use when writing cascading keys\n"
			    "                         Default: value of /sw/kdb/current/namespace or user.\n";
	}
	optionPos = acceptedOptions.find('c');
	if (optionPos!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"plugins-config", no_argument, nullptr, 'c'};
		long_options.push_back(o);
		helpText += "-c --plugins-config      Add a plugin configuration.\n";
	}

	{
		using namespace kdb;
		/*XXX: Step 4: use default from KDB, if available.*/
		KDB kdb;
		KeySet conf;

		for (int i=0; i<=2; ++i)
		{
			std::string dirname;
			switch (i)
			{
			// prefer later dirnames (will override)
			case 0: dirname = "/sw/kdb/current/"; break; // legacy
			case 1: dirname = "/sw/elektra/kdb/#0/%/"; break; // no profile
			case 2: dirname = "/sw/elektra/kdb/#0/current/";
				break; // current profile
			}

			kdb.get(conf, dirname);

			Key k = conf.lookup(dirname+"resolver");
			if (k) resolver = k.get<string>();

			k = conf.lookup(dirname+"format");
			if (k) format = k.get<string>();

			k = conf.lookup(dirname+"plugins");
			if (k) plugins = k.get<string>();

			k = conf.lookup(dirname+"namespace");
			if (k) ns = k.get<string>();

			k = conf.lookup(dirname+"editor");
			if (k) editor = k.get<string>();

			map nks = conf.get<map>(dirname+"namedkeys");
			namedKeys.insert(nks.begin(), nks.end());
		}

	}

	option o = {nullptr, 0, nullptr, 0};
	long_options.push_back(o);

	executable = argv[0];
	commandName = argv[1];

	if (dynamic_cast<ExternalCommand*>(command))
	{
		// do not print to stderr for external commands,
		// we do not know which options they have and
		// otherwise maybe wrong "invalid/unrecognized option"
		// are reported to stderr.
		opterr = 0;
	}

	while ((opt = getopt_long (argc, argv,
					acceptedOptions.c_str(),
					&long_options[0], &index)) != EOF)
	{
		switch (opt)
		{
		/*XXX: Step 5: and now process the option.*/
		case 'a': all = true; break;
		case 'd': debug = true; break;
		case 'e': editor = optarg; break;
		case 'f': force = true; break;
		case 'h': humanReadable = true; break;
		case 'l': load= true; break;
		case 'H': help = true; break;
		case 'i': interactive = true; break;
		case 'n': noNewline = true; break;
		case 't': test = true; break;
		case 'r': recursive = true; break;
		case 'R': resolver = optarg; break;
		case 's': strategy = optarg; break;
		case 'v': verbose = true; break;
		case 'V': version = true; break;
		case 'E': withoutElektra= true; break;
		case '0': null= true; break;
		case '1': first= false; break;
		case '2': second= false; break;
		case '3': third= false; break;
		case 'N': ns = optarg; break;
		case 'c': pluginsConfig = optarg; break;

		default: invalidOpt = true; break;
		}
	}

	optind++; // skip the command name
	while (optind < argc)
	{
		arguments.push_back(argv[optind++]);
	}
}

kdb::KeySet Cmdline::getPluginsConfig(string basepath) const
{
	return kdb::tools::parsePluginArguments(pluginsConfig, basepath);
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

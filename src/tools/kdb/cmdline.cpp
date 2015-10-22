#include <cmdline.hpp>

#include <kdb.hpp>
#include <keysetio.hpp>
#include <kdbconfig.h>

#include <iostream>
#include <vector>
#include <cstdio>
#include <set>

#include <getopt.h>

#include <command.hpp>


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
		option o = {"all", no_argument, 0, 'a'};
		long_options.push_back(o);
		helpText += "-a --all                 Consider all of the keys.\n";
	}
	if (acceptedOptions.find('d')!=string::npos)
	{
		option o = {"debug", no_argument, 0, 'd'};
		long_options.push_back(o);
		helpText += "-d --debug               Give debug information or ask debug questions (in interactive mode).\n";
	}
	if (acceptedOptions.find('f')!=string::npos)
	{
		option o = {"force", no_argument, 0, 'f'};
		long_options.push_back(o);
		helpText += "-f --force               Force the action to be done.\n";
	}
	if (acceptedOptions.find('l')!=string::npos)
	{
		option o = {"load", no_argument, 0, 'f'};
		long_options.push_back(o);
		helpText += "-l --load                Load plugin even if system/elektra is available\n";
	}
	if (acceptedOptions.find('h')!=string::npos)
	{
		option o = {"human-readable", no_argument, 0, 'h'};
		long_options.push_back(o);
		helpText += "-h --human-readable      Print numbers in an human readable way\n";
	}
	if (acceptedOptions.find('H')!=string::npos)
	{
		option o = {"help", no_argument, 0, 'H'};
		long_options.push_back(o);
		helpText += "-H --help                Print help text.\n";
	}
	if (acceptedOptions.find('i')!=string::npos)
	{
		option o = {"interactive", no_argument, 0, 'i'};
		long_options.push_back(o);
		helpText += "-i --interactive         Instead of passing all information by parameters\n";
		helpText += "                         ask the user interactively.\n";
	}
	if (acceptedOptions.find('n')!=string::npos)
	{
		option o = {"no-newline", no_argument, 0, 'n'};
		long_options.push_back(o);
		helpText += "-n --no-newline          Suppress the newline at the end of the output.\n";
	}
	if (acceptedOptions.find('t')!=string::npos)
	{
		option o = {"test", no_argument, 0, 't'};
		long_options.push_back(o);
		helpText += "-t --test                Test.\n";
	}
	if (acceptedOptions.find('r')!=string::npos)
	{
		option o = {"recursive", no_argument, 0, 'r'};
		long_options.push_back(o);
		helpText += "-r --recursive           Work in a recursive mode.\n";
	}
	optionPos = acceptedOptions.find('R');
	if (optionPos!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"resolver", required_argument, 0, 'R'};
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
		option o = {"strategy", required_argument, 0, 's'};
		long_options.push_back(o);
		helpText +=
			"-s --strategy <name>     Specify which strategy should be used to resolve conflicts.\n"  
                        "                         More precisely, strategies are used to handle deviations from the\n"
                        "                         base version of a key.\n"
                        "                         When and which strategies are used and what they do depends\n"
			"                         mostly on the used base KeySet.\n\n"  
			"                         Note: For a two-way merge, the `ours` version of the keys is used\n"
                        "                         in place of `base`\n\n"
			"                         Currently the following strategies exist\n"
			"                           preserve      .. automerge only those keys where just one\n"
			"                                            side deviates from base (default)\n"
			"                           ours          .. like preserve, but in case of conflict use our version\n"
			"                           theirs        .. like preserve, but in case of conflict use their version\n"
			"                           cut           .. primarily used for import. removes existing keys below\n"
			"                                            the import point and always takes the imported version\n"
			"                           import        .. primarily used for import. preserves existing keys if\n"
			"                                            they do not exist in the imported keyset. in all other\n"
			"                                            cases the imported keys have precedence\n"
			"";
	}
	if (acceptedOptions.find('v')!=string::npos)
	{
		option o = {"verbose", no_argument, 0, 'v'};
		long_options.push_back(o);
		helpText += "-v --verbose             Explain what is happening.\n";
	}
	if (acceptedOptions.find('V')!=string::npos)
	{
		option o = {"version", no_argument, 0, 'V'};
		long_options.push_back(o);
		helpText += "-V --version             Print version info.\n";
	}
	if (acceptedOptions.find('E')!=string::npos)
	{
		option o = {"without-elektra", no_argument, 0, 'E'};
		long_options.push_back(o);
		helpText += "-E --without-elektra     Omit the `system/elektra` directory.\n";
	}
	if (acceptedOptions.find('0')!=string::npos)
	{
		option o = {"null", no_argument, 0, '0'};
		long_options.push_back(o);
		helpText += "-0 --null                Use binary 0 termination.\n";
	}
	if (acceptedOptions.find('1')!=string::npos)
	{
		option o = {"first", no_argument, 0, '1'};
		long_options.push_back(o);
		helpText += "-1 --first               Suppress the first column.\n";
	}
	if (acceptedOptions.find('2')!=string::npos)
	{
		option o = {"second", no_argument, 0, '2'};
		long_options.push_back(o);
		helpText += "-2 --second              Suppress the second column.\n";
	}
	if (acceptedOptions.find('3')!=string::npos)
	{
		option o = {"third", no_argument, 0, '3'};
		long_options.push_back(o);
		helpText += "-3 --third               Suppress the third column.\n";
	}
	optionPos = acceptedOptions.find('N');
	if (acceptedOptions.find('N')!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"namespace", required_argument, 0, 'N'};
		long_options.push_back(o);
		helpText += "-N --namespace ns        Specify the namespace to use when writing cascading keys\n"
			    "                         Default: value of /sw/kdb/current/namespace or user.\n";
	}
	optionPos = acceptedOptions.find('c');
	if (optionPos!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"plugins-config", no_argument, 0, 'c'};
		long_options.push_back(o);
		helpText += "-c --plugins-config      Add a plugin configuration.\n";
	}

	{
		using namespace kdb;
		/*XXX: Step 4: use default from KDB, if available.*/
		std::string dirname = "/sw/kdb/current/";
		KDB kdb;
		KeySet conf;
		kdb.get(conf, std::string("user")+dirname);
		kdb.get(conf, std::string("system")+dirname);

		Key k;

		k = conf.lookup(dirname+"resolver");
		if (k) resolver = k.get<string>();

		k = conf.lookup(dirname+"format");
		if (k) format = k.get<string>();

		k = conf.lookup(dirname+"plugins");
		if (k) plugins = k.get<string>();

		k = conf.lookup(dirname+"namespace");
		if (k) ns = k.get<string>();
	}

	option o = {0, 0, 0, 0};
	long_options.push_back(o);

	executable = argv[0];
	commandName = argv[1];

	while ((opt = getopt_long (argc, argv,
					acceptedOptions.c_str(),
					&long_options[0], &index)) != EOF)
	{
		switch (opt)
		{
		/*XXX: Step 5: and now process the option.*/
		case 'a': all = true; break;
		case 'd': debug = true; break;
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
	using namespace kdb;

	string keyName;
	string value;
	KeySet ret;
	istringstream sstream(pluginsConfig);

	// read until the next '=', this will be the keyname
	while (std::getline (sstream, keyName, '='))
	{
		// read until a ',' or the end of line
		// if nothing is read because the '=' is the last character
		// in the config string, consider the value empty
		if (!std::getline (sstream, value, ',')) value = "";

		Key configKey = Key (basepath + keyName, KEY_END);
		configKey.setString (value);
		ret.append (configKey);
	}
	return ret;
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

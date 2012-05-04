#include <cmdline.hpp>
#include <iostream>
#include <vector>
#include <cstdio>

#include <getopt.h>


using namespace std;

Cmdline::Cmdline (int argc, char** argv,
		string const& pAcceptedOptions,
		string const& pHelpText) :
	helpText(pHelpText),
	invalidOpt(false),

	/*XXX: Step 2: initialise your option here.*/
	debug(),
	force(),
	humanReadable(),
	help(),
	interactive(),
	test(),
	recursive(),
	strategy("preserve"),
	verbose(),
	version(),

	executable(),
	command()
{
	extern int optind;
	extern char *optarg;

	int index = 0;
	int opt;

	string acceptedOptions = pAcceptedOptions;
	acceptedOptions += "HV";

	vector<option> long_options;
	/*XXX: Step 3: give it a long name.*/
	if (acceptedOptions.find('d')!=string::npos)
	{
		option o = {"debug", no_argument, 0, 'd'};
		long_options.push_back(o);
		helpText += "-d --debug               give debug information or ask debug questions (in interactive mode)\n";
	}
	if (acceptedOptions.find('f')!=string::npos)
	{
		option o = {"force", no_argument, 0, 'f'};
		long_options.push_back(o);
		helpText += "-f --force               force the action to be done\n";
	}
	if (acceptedOptions.find('h')!=string::npos)
	{
		option o = {"human-readable", no_argument, 0, 'h'};
		long_options.push_back(o);
		helpText += "-h --human-readable      print numbers in an human readable way\n";
	}
	if (acceptedOptions.find('H')!=string::npos)
	{
		option o = {"help", no_argument, 0, 'H'};
		long_options.push_back(o);
		helpText += "-H --help                print help text\n";
	}
	if (acceptedOptions.find('i')!=string::npos)
	{
		option o = {"interactive", no_argument, 0, 'i'};
		long_options.push_back(o);
		helpText += "-i --interactive         instead of passing all information by parameters\n";
		helpText += "                         ask the user interactively\n";
	}
	if (acceptedOptions.find('t')!=string::npos)
	{
		option o = {"test", no_argument, 0, 't'};
		long_options.push_back(o);
		helpText += "-t --test                test\n";
	}
	if (acceptedOptions.find('r')!=string::npos)
	{
		option o = {"recursive", no_argument, 0, 'r'};
		long_options.push_back(o);
		helpText += "-r --recursive           work in a recursive mode\n";
	}
	if (acceptedOptions.find('s')!=string::npos)
	{
		option o = {"strategy", required_argument, 0, 'r'};
		long_options.push_back(o);
		helpText += "-s --strategy <name>     the strategy which should be used on conflicts\n";
		helpText += "                         preserve .. no old key is overwritten\n";
		helpText += "                         overwrite .. overwrite keys with same name\n";
		helpText += "                         cut .. completely cut old keys to make place for new\n";
	}
	if (acceptedOptions.find('v')!=string::npos)
	{
		option o = {"verbose", no_argument, 0, 'v'};
		long_options.push_back(o);
		helpText += "-v --verbose             be more verbose\n";
	}
	if (acceptedOptions.find('V')!=string::npos)
	{
		option o = {"version", no_argument, 0, 'V'};
		long_options.push_back(o);
		helpText += "-V --version             print version info\n";
	}


	option o = {0, 0, 0, 0};
	long_options.push_back(o);

	executable += argv[0];
	command += argv[1];

	while ((opt = getopt_long (argc, argv,
					acceptedOptions.c_str(),
					&long_options[0], &index)) != EOF)
	{
		switch (opt)
		{
		/*XXX: Step 4: and now process the option.*/
		case 'd': debug = true; break;
		case 'f': force = true; break;
		case 'h': humanReadable = true; break;
		case 'H': help = true; break;
		case 'i': interactive = true; break;
		case 't': test = true; break;
		case 'r': recursive = true; break;
		case 's': strategy = optarg; break;
		case 'v': verbose = true; break;
		case 'V': version = true; break;

		default: invalidOpt = true; break;
		}
	}

	optind++; // skip the command name
	while (optind < argc)
	{
		arguments.push_back(argv[optind++]);
	}
}

std::ostream & operator<< (std::ostream & os, Cmdline & cl)
{
	if (cl.invalidOpt)
	{
		os << "Invalid option given\n" << endl;
	}

	os << "Usage: " << cl.executable << " " << cl.command << " ";
	os << cl.helpText;
	return os;
}

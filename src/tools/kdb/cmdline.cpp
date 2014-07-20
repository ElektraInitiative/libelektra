#include <cmdline.hpp>

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
	strategy("auto"),
	overrideBase(),
	verbose(),
	version(),

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
	if (acceptedOptions.find('l')!=string::npos)
	{
		option o = {"load", no_argument, 0, 'f'};
		long_options.push_back(o);
		helpText += "-l --load                load plugin even if system/elektra is available\n";
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
	if (acceptedOptions.find('n')!=string::npos)
	{
		option o = {"no-newline", no_argument, 0, 'n'};
		long_options.push_back(o);
		helpText += "-n --no-newline          suppress the newline at the end of the output\n";
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
	optionPos = acceptedOptions.find('s');
	if (optionPos!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"strategy", required_argument, 0, 's'};
		long_options.push_back(o);
		helpText += "-s --strategy <name>     the strategy which should be used on conflicts\n";
		helpText += "                         preserve .. no old key is overwritten (default)\n";
		helpText += "                         overwrite .. overwrite keys with same name\n";
		helpText += "                         cut .. completely cut at rootkey to make place for new keys\n";
	}
	if (acceptedOptions.find('b') != string::npos)
	{
		option b = {"overrideBase", no_argument, 0, 'b'};
		long_options.push_back(b);
		helpText += "-b --overrideBase        allow overriding the base with the merge result\n";
		helpText += "						  with this option the merge command is no longer idempotent\n";
		helpText += "                         (i.e. repeating the same command multiple times would yield\n";
		helpText += "						  different results, because the base changes every time)";
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

	executable = argv[0];
	commandName = argv[1];

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
		case 'l': load= true; break;
		case 'H': help = true; break;
		case 'i': interactive = true; break;
		case 'n': noNewline = true; break;
		case 't': test = true; break;
		case 'r': recursive = true; break;
		case 's': strategy = optarg; break;
		case 'b': overrideBase = true; break;
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

	os << "Usage: " << cl.executable << " " << cl.commandName << " " << cl.synopsis;
	os << "\n\n" << cl.helpText;
	return os;
}

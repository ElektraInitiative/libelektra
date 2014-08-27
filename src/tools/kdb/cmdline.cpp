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
	resolver("resolver"),
	strategy("preserve"),
	verbose(),
	version(),
	withoutElektra(),

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
	optionPos = acceptedOptions.find('R');
	if (optionPos!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"resolver", required_argument, 0, 'R'};
		long_options.push_back (o);
		helpText +=
				"-R --resolver <name>     the resolver plugin to use\n"
				"                         if no resolver is given, the default resolver is used\n"
				"";
	}
	optionPos = acceptedOptions.find('s');
	if (optionPos!=string::npos)
	{
		acceptedOptions.insert(optionPos+1, ":");
		option o = {"strategy", required_argument, 0, 's'};
		long_options.push_back(o);
		helpText +=
			"-s --strategy <name>     the strategy which should be used on conflicts.\n"
			"                         To be precise, strategies handle deviations from the base\n"
			"                         When and which strategies are used and what they do depends mostly on\n"
			"                         the used base KeySet. For twoway-merge the base stays empty\n"
			"                         Strategies can be chained. That means if one strategy\n"
			"                         is not able to solve a conflict (i.e. conflicting deviations)\n"
			"                         the next strategy in the chain is tried. This happens until the conflict\n"
			"                         is solved or no strategies are left\n"
			"                         For example, if you want to accept all deviations that do not conflict to theirs,\n"
			"                         but use theirs in case of conflict use -s ourvalue,theirs\n"
			"                         Currently the following strategies exist\n"
			"                         preserve      .. no old key is overwritten (default)\n"
			"                         ours          .. use always our version in case of conflict\n"
			"                         theirs        .. use always their version in case of conflict\n"
			"                         base          .. use always the base version in case of conflict\n"
			"                         newkey        .. merge just those keys added by one side only\n"
			"                         ourvalue      .. use our value if theirs is unmodified\n"
			"                         theirvalue    .. use their value if ours is unmodified\n"
			"";
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
	if (acceptedOptions.find('E')!=string::npos)
	{
		option o = {"without-elektra", no_argument, 0, 'E'};
		long_options.push_back(o);
		helpText += "-E --without-elektra     omit system/elektra directory\n";
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
		case 'R': resolver = optarg; break;
		case 's': strategy = optarg; break;
		case 'v': verbose = true; break;
		case 'V': version = true; break;
		case 'E': withoutElektra= true; break;

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

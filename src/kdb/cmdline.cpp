#include <cmdline.hpp>
#include <iostream>
#include <vector>
#include <cstdio>

#include <getopt.h>


using namespace std;

Cmdline::Cmdline (int argc, char** argv,
		unsigned int nrOptions,
		string const& acceptedOptions,
		string const& phelpText) :
	optindex(0), argc(argc), invalidOpt(false),
	/*XXX: Step 2: initialise your option here.*/
	h(false),
	H(false),
	t(false),
	v(false),
	V(false),
	progName(), utilName(),
	helpText(phelpText)
{
	extern int optind;
	// extern char *optarg;

	int index = 0;
	int opt;

	vector<option> long_options;
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
	if (acceptedOptions.find('t')!=string::npos)
	{
		option o = {"test", no_argument, 0, 't'};
		long_options.push_back(o);
		helpText += "-t --test                test\n";
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
	/*XXX: Step 3: give it a long name.*/
	option o = {0, 0, 0, 0};
	long_options.push_back(o);

	progName += argv[0];
	utilName += argv[1];

	while ((opt = getopt_long (argc-1, argv+1,
					acceptedOptions.c_str(),
					&long_options[0], &index)) != EOF)
	{
		switch (opt)
		{
		case 'h': h = true; break;
		case 'H': H = true; break;
		case 't': t = true; break;
		case 'v': v = true; break;
		case 'V': V = true; break;
		/*XXX: Step 4: and now process the option.*/
		default:
			invalidOpt = true;
			break;
		}
	}

	optindex = optind;
}

std::ostream & operator<< (std::ostream & os, Cmdline & cl)
{
	if (cl.invalidOpt)
	{
		os << "Invalid option given\n" << endl;
	}

	os << "Usage: " << cl.progName << " " << cl.utilName << " ";
	os << cl.helpText;
	if (cl.v)
	{
		os << "Nr param: " << cl.param() << endl;
		os << "Nr avail: " << cl.avail() << endl;
	}
	else
	{
		os << "Verbose mode is off" << endl;
	}
	return os;
}

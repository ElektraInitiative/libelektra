#include <cmdline.hpp>
#include <iostream>

#include <getopt.h>

using namespace std;

Cmdline::Cmdline (int argc, char** argv) :
	optindex(0), argc(argc), invalidOpt(false),
	h(false), v(false), t(false),
	progName(), utilName()
{
	extern int optind;
	// extern char *optarg;

	int index = 0;
	int opt;

	struct option long_options [] =
	{
		{"help", no_argument, 0, 'h'},
		{"version", no_argument, 0, 'v'},
		{"test", no_argument, 0, 't'},
		{0, 0, 0, 0}
	};

	progName += argv[0];
	utilName += argv[1];

	while ((opt = getopt_long (argc-1, argv+1, "hvt", long_options, &index)) != EOF)
	{
		switch (opt)
		{
		case 'h':
			h = true;
			break;
		case 'v':
			v = true;
			break;
		case 't':
			t = true;
			break;
		default:
			invalidOpt = true;
			break;
		}
	}

	optindex = optind;
}

void Cmdline::printDebug()
{
	cout << "Nr param: " << param() << endl;
	cout << "Nr avail: " << avail() << endl;
	cout << "optindex: " << optindex << endl;
	cout << "argc: " << argc << endl;
	cout << "error: " << invalidOpt << endl;

}

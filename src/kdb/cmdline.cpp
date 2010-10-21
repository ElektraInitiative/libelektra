#include <cmdline.hpp>
#include <iostream>

#include <getopt.h>

using namespace std;

Cmdline::Cmdline (int argc, char** argv) :
	optindex(0), argc(argc), invalidOpt(false),
	/*XXX: Step 2: initialise your option here.*/
	h(false),
	H(false),
	t(false),
	v(false),
	V(false),
	progName(), utilName()
{
	extern int optind;
	// extern char *optarg;

	int index = 0;
	int opt;

	struct option long_options [] =
	{
		/*XXX: Step 3: give it a long name.*/
		{"human-readable", no_argument, 0, 'h'},
		{"help", no_argument, 0, 'H'},
		{"test", no_argument, 0, 't'},
		{"verbose", no_argument, 0, 'v'},
		{"version", no_argument, 0, 'V'},
		{0, 0, 0, 0}
	};

	progName += argv[0];
	utilName += argv[1];

	/*XXX: Step 4: add the option to the string.*/
	while ((opt = getopt_long (argc-1, argv+1, "hHtvV", long_options, &index)) != EOF)
	{
		/*XXX: Step 5: and now process the option.*/
		switch (opt)
		{
		case 'h': h = true; break;
		case 'H': H = true; break;
		case 't': t = true; break;
		case 'v': v = true; break;
		case 'V': V = true; break;
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

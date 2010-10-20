#ifndef CMDLINE_HPP
#define CMDLINE_HPP

#include <string>

class Cmdline
{
	int optindex;
	int argc;

public:
	Cmdline (int argc, char** argv);
	~Cmdline () {};

	/** At least one of the options was invalid */
	bool invalidOpt;

	/** True if help is needed. */
	bool h;
	/** True to return version info. */
	bool v;
	/** True to run some self tests. */
	bool t;

	/** The path to the kdb tool. */
	std::string progName;
	/** The given name for the current utility.
	  * This is the second parameter. */
	std::string utilName;

	/** Returns first parameter that is not an option.
	  * @param i Return the i-th instead of the first parameter.
	  *
	  * @note Will return the last parameter (argv is null there) for out of range
	  * So make sure to handle the null pointer or check avail() before.
	  *
	  */
	int param(int i=1) {return (optindex+i >= argc) ? argc : (optindex+i);}

	/** Returns the number of parameters given after the options.
	 *
	 * Use param() to get the index to the first of these parameters.
	 */
	int avail() {return argc - optindex - 1;}

	void printDebug();
};

#endif

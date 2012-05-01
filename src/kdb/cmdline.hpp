#ifndef CMDLINE_HPP
#define CMDLINE_HPP

/* Cmdline parser.
 *
 * To add an option there are 5 steps.
 * Beware not to introduce options which already have
 * an meaning in one of the utilities.
 * Please always append the options in alphabetical order
 * with capitals later.
 */


#include <string>

class Cmdline
{
private:
	int optindex;
	int argc;

public:
	Cmdline (int argc, char** argv,
			unsigned int nrAcceptedOptions,
			std::string const& shortAcceptedOptions,
			std::string const& helpText);
	~Cmdline () {};

	/** At least one of the options was invalid */
	bool invalidOpt;

	/*XXX: Step 1: add your option here.*/
	bool h; /*!< True if human readable is preferred. */
	bool H; /*!< True if help is needed. */
	bool t; /*!< True to run some self tests instead of the normal action. */
	bool v; /*!< True to be more verbose. */
	bool V; /*!< True to return version info. */

	/** The path to the kdb tool. */
	std::string progName;
	/** The given name for the current utility.
	  * This is the second parameter. */
	std::string utilName;

	/** The help text to printed out. */
	std::string helpText;

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

};

std::ostream & operator<< (std::ostream & os, Cmdline & cl);

#endif

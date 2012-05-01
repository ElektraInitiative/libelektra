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
#include <vector>

class Cmdline
{
public:
	Cmdline (int argc, char** argv,
			std::string const& shortAcceptedOptions,
			std::string const& helpText);
	~Cmdline () {};

	/** The help text to printed out. */
	std::string helpText;

	/** At least one of the options was invalid */
	bool invalidOpt;

	/*XXX: Step 1: add your option here.*/
	std::string format; /*!< a plugin to be used to format the conf. */
	bool interactive; /*!< Interactive mode. */
	bool recursive; /*!< Recursive mode. */
	bool humanReadable; /*!< Human readable values are preferred. */
	bool help; /*!< Display help instead of the normal action.. */
	bool test; /*!< Run some self tests instead of the normal action. */
	bool verbose; /*!< Be more verbose. */
	bool version; /*!< Return version info instead of the normal action.. */

	/** The path to the kdb exectuable. */
	std::string executable;

	/** The given name for the current command.
	  * This is the second parameter. */
	std::string command;

	/** The arguments given on the commandline. */
	std::vector <std::string> arguments;
};

std::ostream & operator<< (std::ostream & os, Cmdline & cl);

#endif

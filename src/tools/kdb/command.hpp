/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see doc/COPYING or http://www.libelektra.org)
 */

#ifndef COMMAND_HPP
#define COMMAND_HPP

#include <string>
#include <memory>
#include <exception>
#include <stdexcept>

class Cmdline;

/**
 * Base class for any exceptions thrown in a command.
 */
class CommandException : public std::exception
{
public:
	virtual const char* what() const throw() override
	{
		return "A situation had a appeared where the command had to abort";
	}
};

class CommandAbortException : public CommandException
{
	const char * msg;
public:
	CommandAbortException () : msg(0)
	{}
	CommandAbortException (const char * msg_) : msg(msg_)
	{}
	virtual const char* what() const throw() override
	{
		return msg?msg:"Command aborted";
	}
};

class Command
{
public:
	virtual ~Command();

	/**
	  * @return the text representing which short options are needed
	  *         by this command.
	  *
	  * @note the :, which indicates that an optional argument is needed,
	  *       is added automatically.
	  *       HV (for help and version) are automatically added.
	  *       All options are sorted and made unique before passed
	  *       to getopt.
	  *
	  */
	virtual std::string getShortOptions() = 0;

	/**
	  * @return a string describing describe what parameter the
	  *       command needs or an empty line if none.
	  *
	  * The return value should be:
	  *
	  * @code
	  * <name> [<value>]
	  * @endcode
	  *
	  * if you want the help to be printed like:
	  *
	  * @code
	  * Usage: kdb set <name> [<value>]
	  * @endcode
	  *
	  * The string may be empty if no arguments are taken.
	  */
	virtual std::string getSynopsis() = 0;

	/**
	  * @return a one line help text
	  *
	  * The help text is shown in the overview of the command
	  * and as first line of normal help text given by -H or --help
	  *
	  * The sentence should end with an dot.
	  * No newline should occur in the sentence.
	  *
	  * @see getLongHelpText for the other help text lines
	  */
	virtual std::string getShortHelpText() = 0;

	/**
	  * @return a long description of what the command does.
	  *
	  * May contain multiple lines or paragraphs.
	  * A line should not exceed 72 characters.
	  * The text should not start or end with an newline feed.
	  *
	  * @code
	  * Long description what the command does.
	  * Even more explanation.
	  * @endcode
	  *
	  * The long description should not repeat what the
	  * short help text already said, because the
	  * short help text will be the first line
	  * when help is shown.
	  *
	  * @see getShortHelpText for the first line
	  *
	  * The long text should describe all elements
	  * of the synopsis.
	  *
	  * @see getSynopsis for the synopsis
	  *
	  * It should not describe any of the commandline
	  * options, but rather use the cmdline option
	  * parsing system.
	  *
	  * @see getShortOptions to express available options
	  */
	virtual std::string getLongHelpText() = 0;

	/**
	  * Execute the command.
	  *
	  * @pre The options are parsed already.
	  *
	  * You need to take care if there is the correct number
	  * of arguments with the correct contents.
	  * This is needed because of optional parameters.
	  *
	  * @retval 0 if the command could be executed successfully.
	  *
	  * @return a positive number if any other error occurred.
	  *         No help is shown in this situation.
	  *
	  * @throw invalid_argument if the cmdline arguments could
	  *        not be processed successfully.
	  */
	virtual int execute (Cmdline const& cmdline) = 0;
};

typedef std::unique_ptr<Command> CommandPtr;

#endif

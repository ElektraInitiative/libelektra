/**
 * @file
 *
 * @brief
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

#ifndef COMMAND_HPP
#define COMMAND_HPP

#include <exception>
#include <memory>
#include <stdexcept>
#include <string>

class Cmdline;

/**
 * Base class for some of the exceptions thrown in a command, except
 * (see main.cpp for handling):
 * - KDBException for KDB errors
 * - invalid_argument is thrown for argument errors
 */
class CommandException : public std::exception
{
public:
	virtual const char * what () const throw () override
	{
		return "A situation had a appeared where the command had to abort, but no message was given.";
	}

	virtual int errorCode () const throw ()
	{
		return 3;
	}
};

class CommandAbortException : public CommandException
{
	int m_errorCode;
	std::string m_msg;

public:
	CommandAbortException () : m_errorCode (3), m_msg ()
	{
	}

	explicit CommandAbortException (std::string msg, int errorCode = 3) : m_errorCode (errorCode), m_msg (msg)
	{
	}

	virtual const char * what () const throw () override
	{
		return !m_msg.empty () ? m_msg.c_str () :
					 "A situation had occurred where the command had to abort, but no message is available.";
	}

	virtual int errorCode () const throw () override
	{
		return m_errorCode;
	}
};

class Command
{
public:
	virtual ~Command ();

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
	virtual std::string getShortOptions () = 0;

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
	virtual std::string getSynopsis () = 0;

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
	virtual std::string getShortHelpText () = 0;

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
	virtual std::string getLongHelpText () = 0;

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
	virtual int execute (Cmdline const & cmdline) = 0;
};

typedef std::unique_ptr<Command> CommandPtr;

#endif

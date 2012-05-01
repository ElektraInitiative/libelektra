#ifndef COMMAND_HPP
#define COMMAND_HPP

#include <print.hpp>
#include <cmdline.hpp>

#include <string>
#include <exception>
#include <stdexcept>

/**
 * Base class for any exceptions thrown in a command.
 */
class CommandException : public std::exception
{
public:
	virtual const char* what() const throw()
	{
		return "A situation had a appeared where the command had to abort";
	}
};

class CommandAbortException : public CommandException
{
public:
	virtual const char* what() const throw()
	{
		return "Aborted by user request";
	}
};

class Command
{
public:
	virtual ~Command();

	/**
	  * @return the text representing which short options are needed
	  *         by this command.
	  */
	virtual std::string getShortOptions() = 0;

	/**
	  * @return a one line help text
	  *
	  * The help text is shown in the overview of the command
	  * and as first line of normal help text given by -H or --help
	  */
	virtual std::string getShortHelpText() = 0;

	/**
	  * @return a long description of what the command does.
	  *
	  * @note the first line must describe what parameter the
	  *       command needs or an empty line if none.
	  *
	  * @code
	  * \<name\>
	  * Long description what the command does.
	  * Even more explanation.
	  * @endcode
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

#endif

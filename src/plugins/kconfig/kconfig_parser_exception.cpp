#include "./kconfig_parser_exception.hpp"
#include "./base.hpp"
#include "./file_utility.hpp"

#include <sstream>
namespace kconfig
{
KConfigParserException::KConfigParserException (FileUtility const & fileUtility, std::string const & errorMessage)
{
	message = generateErrorMessage (fileUtility.getFilename (), fileUtility.getCurrentLineNumber (), errorMessage);
}

KConfigParserException KConfigParserException::expect (FileUtility & fileUtility, char expected)
{
	return expect (fileUtility, describeCharacter (expected));
}

KConfigParserException KConfigParserException::expect (FileUtility & fileUtility, std::string const & expectedValue)
{
	std::string foundValue{ describeCharacter (fileUtility.peekNextChar ()) };
	return { fileUtility, "Expected " + expectedValue + " character, found " + foundValue };
}

std::string KConfigParserException::generateErrorMessage (std::string const & filename, int lineNumber, std::string const & message)
{
	std::stringstream errorMessage;
	errorMessage << "Error while parsing " << filename;
	if (lineNumber != 0)
	{
		errorMessage << " at line " << lineNumber;
	}
	errorMessage << ": ";
	errorMessage << message;
	return errorMessage.str ();
}

std::string KConfigParserException::describeCharacter (char c)
{
	if (c == character_newline || c == character_carriage_return)
	{
		return "new line";
	}
	else
	{
		std::string ret{ "`" };
		ret.push_back (c);
		ret.push_back ('`');
		return ret;
	}
}

std::string KConfigParserException::getMessage () const
{
	return message;
}
}

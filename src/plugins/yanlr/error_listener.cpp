/**
 * @file
 *
 * @brief an error listener reacting to mismatches of the grammar defined in `YAML.g4`
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------

#include <iostream>

#include <kdbmacros.h>

#include "error_listener.hpp"

using std::to_string;

using antlr4::CommonTokenStream;

// -- Functions ----------------------------------------------------------------

namespace
{

using std::string;

using antlr4::Recognizer;
using antlr4::Token;

/**
 * @brief This function returns a Clang-like error message for a given error.
 *
 * @param recognizer This parameter stores the current recognizer used to
 *                   parse the input.
 * @param offendingSymbol This token caused the failure of the parsing
 *                        process.
 * @param line This number specifies the line where the parsing process
 *             failed.
 * @param charPositionInLine This number specifies the character position in
 *                           `line`, where the parsing process failed.
 * @param prefix This variable stores as prefix that this function prepends
 *               to every line of the visualized error message.
 *
 * @return A string representation of the error
 */
string visualizeError (Recognizer * recognizer, Token * offendingSymbol, size_t const line, size_t const charPositionInLine,
		       string const & prefix)
{
	CommonTokenStream * tokens = dynamic_cast<CommonTokenStream *> (recognizer->getInputStream ());
	string input = tokens->getTokenSource ()->getInputStream ()->toString ();

	string::size_type start = 0;
	string::size_type end = 0;
	for (size_t currentLine = 1; currentLine <= line; currentLine++)
	{
		size_t offset = (end == 0 ? 0 : 1);
		start = end + offset;
		end = input.find ("\n", end + offset);
	}

	string errorLine = input.substr (start, end - start);

	errorLine = prefix + errorLine + "\n" + prefix + string (charPositionInLine - 1, ' ');
	start = offendingSymbol->getStartIndex ();
	end = offendingSymbol->getStopIndex ();
	for (size_t current = start; current <= end; current++)
	{
		errorLine += "^";
	}

	return errorLine;
}
}

// -- Class --------------------------------------------------------------------

namespace yanlr
{

using antlr4::Recognizer;

/**
 * @brief This constructor creates a new error listener using the given arguments.
 *
 * @param errorSource This text stores an identifier, usually the filename, that identifies the source of an error.
 */
ErrorListener::ErrorListener (string const & errorSource)
{
	source = errorSource;
}

/**
 * @brief This method will be called if the parsing process fails.
 *
 * @param recognizer This parameter stores the current recognizer used to
 *                   parse the input.
 * @param offendingSymbol This token caused the failure of the parsing
 *                        process.
 * @param line This number specifies the line where the parsing process
 *             failed.
 * @param charPositionInLine This number specifies the character position in
 *                           `line`, where the parsing process failed.
 * @param message This text describes the parsing failure.
 * @param error This parameter stores the exception caused by the parsing
 *              failure.
 */
void ErrorListener::syntaxError (Recognizer * recognizer, Token * offendingSymbol, size_t line, size_t charPositionInLine,
				 const std::string & message, std::exception_ptr error ELEKTRA_UNUSED)
{
	auto location = source + ":" + to_string (line) + ":" + to_string (charPositionInLine) + ": ";
	auto indent = string (location.length (), ' ');
	errorMessage += "\n" + location + message + "\n";
	errorMessage += visualizeError (recognizer, offendingSymbol, line, charPositionInLine, indent);
}

/**
 * @brief This method returns the last error message saved by the error listener.
 *
 * @return A string containing an error message produced by the parser
 */
char const * ErrorListener::message ()
{
	return errorMessage.c_str ();
}
}

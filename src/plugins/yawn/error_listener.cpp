/**
 * @file
 *
 * @brief This file implements an error listener for the YAML parser.
 *
 * @copyright BSD License (see LICENSE.md or https://www.libelektra.org)
 */

// -- Imports ------------------------------------------------------------------------------------------------------------------------------

#include <memory>
#include <string>

#include <kdblogger.h>
#include <kdbmacros.h>

#include <yaep.h>

#include "error_listener.hpp"
#include "token.hpp"

using std::string;
using std::to_string;
using std::unique_ptr;

// -- Class --------------------------------------------------------------------------------------------------------------------------------

namespace yawn
{

/**
 * @brief This method returns a Clang-like error message for a given error.
 *
 * @param location This parameter stores the location of the error.
 * @param prefix This variable stores as prefix that this function prepends
 *               to every line of the visualized error message.
 *
 * @return A string representation of the error
 */
string ErrorListener::visualizeError (Location const & location, string const & prefix)
{
	string::size_type start = 0;
	string::size_type end = 0;
	for (size_t currentLine = 1; currentLine <= location.begin.line; currentLine++)
	{
		size_t offset = (end == 0 ? 0 : 1);
		start = end + offset;
		end = input.find ("\n", end + offset);
	}

	string errorLine = input.substr (start, end - start);

	errorLine = prefix + errorLine + "\n" + prefix + string (location.begin.column - 1, ' ');
	// We assume that an error does not span more than one line
	start = location.begin.column;
	end = location.end.column - 1;
	errorLine += "^"; // Show at least one caret, even if the token is 0 characters long
	for (size_t current = start; current < end; current++)
	{
		errorLine += "^";
	}

	return errorLine;
}

/**
 * @brief This constructor creates a new error listener using the given arguments.
 *
 * @param errorSource This text stores an identifier, usually the filename, that identifies the source of an error.
 * @param text This variable stores the content of `errorSource` as UTF-8 encoded string.
 */
ErrorListener::ErrorListener (string const & errorSource, string const & text)
{
	source = errorSource;
	input = text;
}

/**
 * @brief This method reacts to syntax errors reported by YAEP’s parsing
 *        engine.
 *
 * @param errorTokenNumber This number specifies the token where the error
 *                         occurred.
 * @param errorTokenData This variable stores the data contained in
 *                       `errorToken`.
 * @param ignoredToken This number specifies the first token that was ignored
 *                     during error recovery.
 * @param ignoredTokenData This variable stores the data contained in
 *                         `ignoredToken`.
 * @param recoveredToken This number specifies the first included token after
 *                       the error recovery has taken place.
 * @param recoveredTokenData This variable stores the data contained in
 *                           `recoveredToken`.
 */
void ErrorListener::syntaxError (int errorTokenNumber ELEKTRA_UNUSED, void * errorTokenData, int ignoredToken ELEKTRA_UNUSED,
				 void * ignoredTokenData ELEKTRA_UNUSED, int recoveredToken ELEKTRA_UNUSED,
				 void * recoveredTokenData ELEKTRA_UNUSED)
{
	errors++;
	auto token = **static_cast<unique_ptr<Token> *> (errorTokenData);

	auto location = token.getLocation ();

	auto position = source + ":" + to_string (location.begin.line) + ":" + to_string (location.begin.column) + ": ";
	auto explanation = "Syntax error on input “" + to_string (token) + "”";
	auto indent = string (position.length (), ' ');

	message += "\n" + position + explanation + "\n";
	message += visualizeError (location, indent);

#ifdef HAVE_LOGGER
	if (ignoredTokenData != nullptr)
	{
		auto data = **static_cast<unique_ptr<Token> *> (ignoredTokenData);
		ELEKTRA_LOG_DEBUG ("Ignored token number %d: %s", ignoredToken, to_string (data).c_str ());
	}
	if (recoveredTokenData != nullptr)
	{
		auto data = **static_cast<unique_ptr<Token> *> (recoveredTokenData);
		ELEKTRA_LOG_DEBUG ("Recovered on token number %d: %s", recoveredToken, to_string (data).c_str ());
	}
#endif
}

/**
 * @brief This method returns a description of the last syntax error.
 *
 * @return A text describing the last error
 */
string ErrorListener::getErrorMessage () const
{
	return message;
}

/**
 * @brief This method returns the number of syntax errors reported by YAEP.
 *
 * @return The number of syntax errors found in the parsed input
 */
size_t ErrorListener::getNumberOfErrors () const
{
	return errors;
}

} // namespace yawn
